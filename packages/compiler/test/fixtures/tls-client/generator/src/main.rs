use ring::{aead, hmac};
use rustls::crypto::{ring as rustls_ring, CryptoProvider, SupportedKxGroup};
use rustls::pki_types::{CertificateDer, PrivateKeyDer};
use rustls::server::WebPkiClientVerifier;
use rustls::{KeyLog, RootCertStore, ServerConfig, ServerConnection, SupportedCipherSuite};
use std::fs::{self, File};
use std::io::{BufReader, Cursor, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use x25519_dalek::{PublicKey, StaticSecret};

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn load_certs(path: &Path) -> Vec<CertificateDer<'static>> {
    rustls_pemfile::certs(&mut BufReader::new(File::open(path).unwrap()))
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
}

fn load_key(path: &Path) -> PrivateKeyDer<'static> {
    rustls_pemfile::private_key(&mut BufReader::new(File::open(path).unwrap()))
        .unwrap()
        .unwrap()
}

#[derive(Debug, Default)]
struct CaptureKeyLog(Mutex<Vec<(String, Vec<u8>, Vec<u8>)>>);

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

impl KeyLog for CaptureKeyLog {
    fn log(&self, label: &str, client_random: &[u8], secret: &[u8]) {
        self.0
            .lock()
            .unwrap()
            .push((label.to_owned(), client_random.to_vec(), secret.to_vec()));
    }
}

impl CaptureKeyLog {
    fn write(&self, path: &Path) {
        let mut file = File::create(path).unwrap();
        for (label, random, secret) in self.0.lock().unwrap().iter() {
            writeln!(file, "{label} {} {}", hex(random), hex(secret)).unwrap();
        }
    }

    fn secret(&self, wanted: &str) -> Vec<u8> {
        self.0
            .lock()
            .unwrap()
            .iter()
            .find(|(label, _, _)| label == wanted)
            .unwrap()
            .2
            .clone()
    }
}

#[derive(Clone, Copy)]
enum Suite {
    ChaCha20Sha256,
    Aes128Sha256,
    Aes256Sha384,
}

impl Suite {
    fn supported(self) -> SupportedCipherSuite {
        match self {
            Self::ChaCha20Sha256 => rustls_ring::cipher_suite::TLS13_CHACHA20_POLY1305_SHA256,
            Self::Aes128Sha256 => rustls_ring::cipher_suite::TLS13_AES_128_GCM_SHA256,
            Self::Aes256Sha384 => rustls_ring::cipher_suite::TLS13_AES_256_GCM_SHA384,
        }
    }

    fn hmac(self) -> hmac::Algorithm {
        match self {
            Self::Aes256Sha384 => hmac::HMAC_SHA384,
            _ => hmac::HMAC_SHA256,
        }
    }

    fn key_length(self) -> usize {
        match self {
            Self::Aes128Sha256 => 16,
            _ => 32,
        }
    }

    fn aead(self) -> &'static aead::Algorithm {
        match self {
            Self::ChaCha20Sha256 => &aead::CHACHA20_POLY1305,
            Self::Aes128Sha256 => &aead::AES_128_GCM,
            Self::Aes256Sha384 => &aead::AES_256_GCM,
        }
    }
}

fn hkdf_expand_label(suite: Suite, secret: &[u8], label: &[u8], length: usize) -> Vec<u8> {
    let mut info = vec![(length >> 8) as u8, length as u8, (6 + label.len()) as u8];
    info.extend_from_slice(b"tls13 ");
    info.extend_from_slice(label);
    info.push(0);
    info.push(1);
    let key = hmac::Key::new(suite.hmac(), secret);
    hmac::sign(&key, &info).as_ref()[..length].to_vec()
}

fn application_record(suite: Suite, traffic_secret: &[u8], plaintext: &[u8]) -> Vec<u8> {
    let key_bytes = hkdf_expand_label(suite, traffic_secret, b"key", suite.key_length());
    let iv = hkdf_expand_label(suite, traffic_secret, b"iv", 12);
    let payload_length = plaintext.len() + 1 + 16;
    let header = [23, 3, 3, (payload_length >> 8) as u8, payload_length as u8];
    let mut body = plaintext.to_vec();
    body.push(23);
    let key = aead::LessSafeKey::new(aead::UnboundKey::new(suite.aead(), &key_bytes).unwrap());
    key.seal_in_place_append_tag(
        aead::Nonce::try_assume_unique_for_key(&iv).unwrap(),
        aead::Aad::from(header),
        &mut body,
    )
    .unwrap();
    let mut record = header.to_vec();
    record.extend_from_slice(&body);
    record
}

fn push_u16(out: &mut Vec<u8>, value: usize) {
    out.push((value >> 8) as u8);
    out.push(value as u8);
}

fn extension(out: &mut Vec<u8>, kind: usize, body: &[u8]) {
    push_u16(out, kind);
    push_u16(out, body.len());
    out.extend_from_slice(body);
}

const SILK_P256_PUBLIC: [u8; 65] = [
    4, 234, 210, 27, 23, 40, 194, 71, 42, 198, 165, 43, 213, 224, 115, 205, 49, 121, 201, 204, 208,
    155, 199, 50, 93, 156, 157, 33, 205, 15, 49, 123, 45, 28, 60, 13, 173, 0, 180, 242, 15, 209,
    184, 159, 184, 140, 47, 235, 223, 153, 38, 146, 19, 169, 182, 40, 164, 122, 95, 226, 106, 158,
    197, 124, 78,
];

// Mirrors the committed Silk scripted provider: Client.random is 2..=33, the initial X25519
// draw is 34..=65, and the retry P-256 scalar is 66..=97. This is test-only deterministic
// entropy and is never a production provider.
fn silk_client_hello(retry_p256: bool, alpn: bool, server_name: Option<&[u8]>) -> Vec<u8> {
    let mut scalar = [0u8; 32];
    for (index, byte) in scalar.iter_mut().enumerate() {
        *byte = (index + 34) as u8;
    }
    let x25519_public = PublicKey::from(&StaticSecret::from(scalar));
    let mut body = Vec::new();
    push_u16(&mut body, 0x0303);
    body.extend(2u8..=33u8);
    body.push(0);
    push_u16(&mut body, 6);
    for suite in [0x1303, 0x1301, 0x1302] {
        push_u16(&mut body, suite);
    }
    body.extend_from_slice(&[1, 0]);

    let mut extensions = Vec::new();
    if let Some(name) = server_name {
        let mut sni = Vec::new();
        push_u16(&mut sni, name.len() + 3);
        sni.push(0);
        push_u16(&mut sni, name.len());
        sni.extend_from_slice(name);
        extension(&mut extensions, 0, &sni);
    }
    extension(&mut extensions, 43, &[2, 3, 4]);
    extension(&mut extensions, 13, &[0, 4, 4, 3, 8, 4]);
    extension(&mut extensions, 50, &[0, 6, 4, 3, 8, 4, 4, 1]);
    extension(&mut extensions, 10, &[0, 4, 0, 29, 0, 23]);
    let public: &[u8] = if retry_p256 {
        &SILK_P256_PUBLIC
    } else {
        x25519_public.as_bytes()
    };
    let mut share = Vec::new();
    push_u16(&mut share, public.len() + 4);
    push_u16(&mut share, if retry_p256 { 23 } else { 29 });
    push_u16(&mut share, public.len());
    share.extend_from_slice(public);
    extension(&mut extensions, 51, &share);
    if alpn {
        extension(&mut extensions, 16, &[0, 3, 2, b'h', b'2']);
    }
    push_u16(&mut body, extensions.len());
    body.extend_from_slice(&extensions);

    let mut handshake = vec![
        1,
        (body.len() >> 16) as u8,
        (body.len() >> 8) as u8,
        body.len() as u8,
    ];
    handshake.extend_from_slice(&body);
    let mut record = vec![
        22,
        3,
        1,
        (handshake.len() >> 8) as u8,
        handshake.len() as u8,
    ];
    record.extend_from_slice(&handshake);
    record
}

struct Capture<'a> {
    id: &'a str,
    suite: Suite,
    group: &'static dyn SupportedKxGroup,
    chain: &'a str,
    key: &'a str,
    alpn_client: bool,
    alpn_server: bool,
    client_auth_request: bool,
    retry_p256: bool,
    server_name: Option<&'a [u8]>,
}

fn drain(server: &mut ServerConnection) -> Vec<u8> {
    let mut output = Vec::new();
    while server.wants_write() {
        assert!(server.write_tls(&mut output).unwrap() > 0);
    }
    output
}

fn feed(server: &mut ServerConnection, input: &[u8]) -> Vec<u8> {
    assert_eq!(
        server.read_tls(&mut Cursor::new(input)).unwrap(),
        input.len()
    );
    server.process_new_packets().unwrap();
    drain(server)
}

fn generate(root: &Path, capture: Capture<'_>) {
    let key_log = Arc::new(CaptureKeyLog::default());
    let provider = Arc::new(CryptoProvider {
        cipher_suites: vec![capture.suite.supported()],
        kx_groups: vec![capture.group],
        ..rustls_ring::default_provider()
    });
    let builder = ServerConfig::builder_with_provider(provider.clone())
        .with_protocol_versions(&[&rustls::version::TLS13])
        .unwrap();
    let builder = if capture.client_auth_request {
        let mut roots = RootCertStore::empty();
        roots
            .add(load_certs(&root.join("keys/root-cert.pem")).remove(0))
            .unwrap();
        let verifier = WebPkiClientVerifier::builder_with_provider(Arc::new(roots), provider)
            .allow_unauthenticated()
            .build()
            .unwrap();
        builder.with_client_cert_verifier(verifier)
    } else {
        builder.with_no_client_auth()
    };
    let mut config = builder
        .with_single_cert(
            load_certs(&root.join(capture.chain)),
            load_key(&root.join(capture.key)),
        )
        .unwrap();
    if capture.alpn_server {
        config.alpn_protocols = vec![b"h2".to_vec()];
    }
    config.key_log = key_log.clone();
    let mut server = ServerConnection::new(Arc::new(config)).unwrap();
    let hello = silk_client_hello(false, capture.alpn_client, capture.server_name);
    let first = feed(&mut server, &hello);
    let mut flight = first.clone();
    if capture.retry_p256 {
        File::create(root.join(format!("captures/{}-server-retry.bin", capture.id)))
            .unwrap()
            .write_all(&first)
            .unwrap();
        let retry_hello = silk_client_hello(true, capture.alpn_client, capture.server_name);
        File::create(root.join(format!("captures/{}-client-retry.bin", capture.id)))
            .unwrap()
            .write_all(&retry_hello)
            .unwrap();
        flight = feed(&mut server, &retry_hello);
    }
    let application_secret = key_log.secret("SERVER_TRAFFIC_SECRET_0");
    flight.extend_from_slice(&application_record(
        capture.suite,
        &application_secret,
        b"coalesced authenticated plaintext",
    ));
    key_log.write(&root.join(format!("captures/{}-keylog.TEST-ONLY.txt", capture.id)));
    File::create(root.join(format!("captures/{}-server-flight.bin", capture.id)))
        .unwrap()
        .write_all(&flight)
        .unwrap();
    File::create(root.join(format!("captures/{}-client-hello.bin", capture.id)))
        .unwrap()
        .write_all(&hello)
        .unwrap();
    eprintln!(
        "{}: {} hello, {} first, {} final",
        capture.id,
        hello.len(),
        first.len(),
        flight.len()
    );
}

fn main() {
    let root = fixture_root();
    fs::create_dir_all(root.join("captures")).unwrap();
    generate(
        &root,
        Capture {
            id: "rsa-x25519",
            suite: Suite::ChaCha20Sha256,
            group: rustls_ring::kx_group::X25519,
            chain: "keys/rsa-leaf-chain.pem",
            key: "keys/rsa-leaf-key.TEST-ONLY.pem",
            alpn_client: false,
            alpn_server: false,
            client_auth_request: false,
            retry_p256: false,
            server_name: Some(b"example.com"),
        },
    );
    generate(
        &root,
        Capture {
            id: "rsa-x25519-wrong-name",
            suite: Suite::ChaCha20Sha256,
            group: rustls_ring::kx_group::X25519,
            chain: "keys/rsa-leaf-chain.pem",
            key: "keys/rsa-leaf-key.TEST-ONLY.pem",
            alpn_client: false,
            alpn_server: false,
            client_auth_request: false,
            retry_p256: false,
            server_name: Some(b"wrong.example"),
        },
    );
    generate(
        &root,
        Capture {
            id: "rsa-x25519-ip",
            suite: Suite::ChaCha20Sha256,
            group: rustls_ring::kx_group::X25519,
            chain: "keys/rsa-leaf-chain.pem",
            key: "keys/rsa-leaf-key.TEST-ONLY.pem",
            alpn_client: false,
            alpn_server: false,
            client_auth_request: false,
            retry_p256: false,
            server_name: None,
        },
    );
    generate(
        &root,
        Capture {
            id: "ecdsa-x25519-aes128",
            suite: Suite::Aes128Sha256,
            group: rustls_ring::kx_group::X25519,
            chain: "keys/ecdsa-leaf-chain.pem",
            key: "keys/ecdsa-leaf-key.TEST-ONLY.pem",
            alpn_client: true,
            alpn_server: true,
            client_auth_request: true,
            retry_p256: false,
            server_name: Some(b"example.com"),
        },
    );
    generate(
        &root,
        Capture {
            id: "ecdsa-x25519-aes128-no-alpn",
            suite: Suite::Aes128Sha256,
            group: rustls_ring::kx_group::X25519,
            chain: "keys/ecdsa-leaf-chain.pem",
            key: "keys/ecdsa-leaf-key.TEST-ONLY.pem",
            alpn_client: true,
            alpn_server: false,
            client_auth_request: false,
            retry_p256: false,
            server_name: Some(b"example.com"),
        },
    );
    generate(
        &root,
        Capture {
            id: "ecdsa-p256-aes256",
            suite: Suite::Aes256Sha384,
            group: rustls_ring::kx_group::SECP256R1,
            chain: "keys/ecdsa-leaf-chain.pem",
            key: "keys/ecdsa-leaf-key.TEST-ONLY.pem",
            alpn_client: false,
            alpn_server: false,
            client_auth_request: false,
            retry_p256: true,
            server_name: Some(b"example.com"),
        },
    );
}
