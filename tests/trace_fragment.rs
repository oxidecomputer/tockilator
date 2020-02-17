/*
 * Copyright 2020 Oxide Computer Company
 */

//! Integration tests using a canned fragment of trace logs.
//!
//! These tests exercise the Tockilator crate using the same interface that main
//! uses.

use std::io::{BufRead, BufReader};

use tockilator::*;

/// This just checks to see that a fragment of trace parses correctly.
#[test]
fn check_parse() {
    let trace: &[u8] = include_bytes!("trace-fragment.txt");
    let line_count = BufReader::new(trace).lines().count();

    let mut tockilator = Tockilator::default();
    let mut callback_invocations = 0;
    tockilator.trace(BufReader::new(trace), |_| {
        callback_invocations += 1;
        Ok(())
    }).unwrap();
    assert_eq!(callback_invocations, line_count - 1);
}
