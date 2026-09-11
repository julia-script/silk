//! Temporary in-crate oracle module installed by verify-rustls-fixtures.sh.

use std::prelude::v1::*;

use crate::crypto::ring::cipher_suite::{
    TLS13_AES_256_GCM_SHA384, TLS13_CHACHA20_POLY1305_SHA256,
};
use crate::crypto::tls13::OkmBlock;
use crate::enums::{ContentType, ProtocolVersion};
use crate::msgs::message::{OutboundChunks, OutboundPlainMessage};
use crate::suites::SupportedCipherSuite;
use crate::tls13::key_schedule::{derive_traffic_iv, derive_traffic_key};

fn record(
    suite: SupportedCipherSuite,
    secret: &[u8],
    content_type: ContentType,
    content: &[u8],
) -> Vec<u8> {
    let suite = suite.tls13().expect("TLS 1.3 suite");
    let secret = OkmBlock::new(secret);
    let expander = suite.hkdf_provider.expander_for_okm(&secret);
    let key = derive_traffic_key(expander.as_ref(), suite.aead_alg);
    let iv = derive_traffic_iv(expander.as_ref());
    let mut encrypter = suite.aead_alg.encrypter(key, iv);
    encrypter
        .encrypt(
            OutboundPlainMessage {
                typ: content_type,
                version: ProtocolVersion::TLSv1_3,
                payload: OutboundChunks::Single(content),
            },
            0,
        )
        .expect("record encryption")
        .encode()
}

#[test]
fn jul171_record_fixture() {
    let aes256 = record(
        TLS13_AES_256_GCM_SHA384,
        &(0u8..48).collect::<Vec<_>>(),
        ContentType::ApplicationData,
        b"Silk AES-256 record",
    );
    let expected_aes256 = hex::decode(
        "17030300241b5e1357354cf3d6bcf660d0df281136aaa8d973f02e41b06a466cf244d5c0ca6697c525",
    )
    .unwrap();
    assert_eq!(aes256, expected_aes256);
    println!("independent-aes256-application={}", hex::encode(aes256));

    let chacha = record(
        TLS13_CHACHA20_POLY1305_SHA256,
        &(0u8..32).map(|value| 255 - value).collect::<Vec<_>>(),
        ContentType::Alert,
        &[1, 0],
    );
    let expected_chacha =
        hex::decode("1703030013e0b73ac1d55a107ef10280958b26be6111c244").unwrap();
    assert_eq!(chacha, expected_chacha);
    println!("independent-chacha-alert={}", hex::encode(chacha));
}
