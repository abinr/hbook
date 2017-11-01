#!/bin/bash

asciidoctor -a highlightjsdir=highlight \
    -o abinr_hbook.html \
    index.adoc

firefox abinr_hbook.html &
