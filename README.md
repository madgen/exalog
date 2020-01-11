# Exalog: Datalog as a library

[![Build Status](https://travis-ci.com/madgen/exalog.svg?branch=master)](https://travis-ci.com/madgen/exalog)

This project provides a Datalog backend as a library written in Haskell. It is meant to facilitate implementation of Datalog progeny that can compile down to range restricted Datalog wih perfect models.

What we provide:

 - Semi-Naïve evaluation engine
 - Stratifier
 - Foreign predicate support (from Haskell)
 - Data provenance tracking
 - Range restriction checker
 - Well-modedness checker (sufficient binding for foreign predicates)
 - Dataflow repair (achieve range restriction and well-modedness via program transformation)
 - Adornment transformation
 - Pretty printer

The road map:

 - Incremental evaluation
 - Type-level enforcement of range restriction and well-modedness
 - Inliner
 - Magic set transformation
 - Deduplicator
 - Interface files
