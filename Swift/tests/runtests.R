#!/usr/bin/env Rscript

library(Swift)

if (!Swift:::prereqTest()) {
    stop("System requirements for Swift not all available!")
}

#TODO: take command line options to setup options
Swift:::runAllSwiftTests()
