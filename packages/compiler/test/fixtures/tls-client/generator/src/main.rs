use rustls::crypto::ring as rustls_ring;
use ring::{aead, hmac};
use rustls::pki_types::{CertificateDer, PrivateKeyDer};
use rustls::{KeyLog, ServerConfig, ServerConnection};
use std::fs::{self, File};
use std::io::{BufReader, Cursor, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use x25519_dalek::{PublicKey, StaticSecret};

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_path_buf()
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
        self.0.lock().unwrap().push((label.to_owned(), client_random.to_vec(), secret.to_vec()));
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
        self.0.lock().unwrap().iter()
            .find(|(label, _, _)| label == wanted)
            .unwrap().2.clone()
    }
}

fn hkdf_expand_label(secret: &[u8], label: &[u8], length: usize) -> Vec<u8> {
    let mut info = vec![(length >> 8) as u8, length as u8, (6 + label.len()) as u8];
    info.extend_from_slice(b"tls13 ");
    info.extend_from_slice(label);
    info.push(0);
    info.push(1);
    let key = hmac::Key::new(hmac::HMAC_SHA256, secret);
    hmac::sign(&key, &info).as_ref()[..length].to_vec()
}

fn application_record(traffic_secret: &[u8], plaintext: &[u8]) -> Vec<u8> {
    let key_bytes = hkdf_expand_label(traffic_secret, b"key", 32);
    let iv = hkdf_expand_label(traffic_secret, b"iv", 12);
    let payload_length = plaintext.len() + 1 + 16;
    let header = [23, 3, 3, (payload_length >> 8) as u8, payload_length as u8];
    let mut body = plaintext.to_vec();
    body.push(23);
    let key = aead::LessSafeKey::new(
        aead::UnboundKey::new(&aead::CHACHA20_POLY1305, &key_bytes).unwrap(),
    );
    key.seal_in_place_append_tag(
        aead::Nonce::try_assume_unique_for_key(&iv).unwrap(),
        aead::Aad::from(header),
        &mut body,
    ).unwrap();
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

// Mirrors the committed Silk scripted provider: Client.random is 2..=33 and the X25519 draw is
// 34..=65. This is test-only deterministic entropy and is never a production provider.
fn silk_client_hello() -> Vec<u8> {
    let mut scalar = [0u8; 32];
    for (index, byte) in scalar.iter_mut().enumerate() {
        *byte = (index + 34) as u8;
    }
    let public = PublicKey::from(&StaticSecret::from(scalar));
    let mut body = Vec::new();
    push_u16(&mut body, 0x0303);
    body.extend(2u8..=33u8);
    body.push(0);
    push_u16(&mut body, 6);
    for suite in [0x1303, 0x1301, 0x1302] { push_u16(&mut body, suite); }
    body.extend_from_slice(&[1, 0]);

    let mut extensions = Vec::new();
    let name = b"example.com";
    let mut sni = Vec::new();
    push_u16(&mut sni, name.len() + 3);
    sni.push(0);
    push_u16(&mut sni, name.len());
    sni.extend_from_slice(name);
    extension(&mut extensions, 0, &sni);
    extension(&mut extensions, 43, &[2, 3, 4]);
    extension(&mut extensions, 13, &[0, 4, 4, 3, 8, 4]);
    extension(&mut extensions, 50, &[0, 6, 4, 3, 8, 4, 4, 1]);
    extension(&mut extensions, 10, &[0, 4, 0, 29, 0, 23]);
    let mut share = Vec::new();
    push_u16(&mut share, 36);
    push_u16(&mut share, 29);
    push_u16(&mut share, 32);
    share.extend_from_slice(public.as_bytes());
    extension(&mut extensions, 51, &share);
    push_u16(&mut body, extensions.len());
    body.extend_from_slice(&extensions);

    let mut handshake = vec![1, (body.len() >> 16) as u8, (body.len() >> 8) as u8, body.len() as u8];
    handshake.extend_from_slice(&body);
    let mut record = vec![22, 3, 1, (handshake.len() >> 8) as u8, handshake.len() as u8];
    record.extend_from_slice(&handshake);
    record
}

fn main() {
    let root = fixture_root();
    let chain = load_certs(&root.join("keys/rsa-leaf-chain.pem"));
    let key = load_key(&root.join("keys/rsa-leaf-key.TEST-ONLY.pem"));
    fs::create_dir_all(root.join("captures")).unwrap();
    let key_log = Arc::new(CaptureKeyLog::default());
    let mut config = ServerConfig::builder_with_provider(Arc::new(rustls_ring::default_provider()))
        .with_protocol_versions(&[&rustls::version::TLS13])
        .unwrap()
        .with_no_client_auth()
        .with_single_cert(chain, key)
        .unwrap();
    config.key_log = key_log.clone();
    let mut server = ServerConnection::new(Arc::new(config)).unwrap();
    let hello = silk_client_hello();
    server.read_tls(&mut Cursor::new(&hello)).unwrap();
    server.process_new_packets().unwrap();
    let mut flight = Vec::new();
    server.write_tls(&mut flight).unwrap();
    let application_secret = key_log.secret("SERVER_TRAFFIC_SECRET_0");
    flight.extend_from_slice(&application_record(
        &application_secret,
        b"coalesced authenticated plaintext",
    ));
    key_log.write(&root.join("captures/rsa-x25519-keylog.TEST-ONLY.txt"));
    File::create(root.join("captures/rsa-x25519-server-flight.bin"))
        .unwrap()
        .write_all(&flight)
        .unwrap();
    File::create(root.join("captures/rsa-x25519-client-hello.bin"))
        .unwrap()
        .write_all(&hello)
        .unwrap();
    eprintln!("captured {} client and {} server bytes", hello.len(), flight.len());
}
