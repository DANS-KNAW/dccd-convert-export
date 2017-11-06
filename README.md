dccd-convert-export
===========
[![Build Status](https://travis-ci.org/DANS-KNAW/dccd-convert-export.png?branch=master)](https://travis-ci.org/DANS-KNAW/dccd-convert-export)

<!-- Remove this comment and extend the descriptions below -->


SYNOPSIS
--------

    dccd-convert-export (synopsis of command line parameters)
    dccd-convert-export (... possibly multiple lines for subcommands)


DESCRIPTION
-----------

Convert DCCD export to multi-deposit


ARGUMENTS
---------

    Options:

        --help      Show help message
        --version   Show version of this program

    Subcommand: run-service - Starts Dccd Convert Export as a daemon that services HTTP requests
        --help   Show help message
    ---

EXAMPLES
--------

    dccd-convert-export -o value


INSTALLATION AND CONFIGURATION
------------------------------


1. Unzip the tarball to a directory of your choice, typically `/usr/local/`
2. A new directory called dccd-convert-export-<version> will be created
3. Add the command script to your `PATH` environment variable by creating a symbolic link to it from a directory that is
   on the path, e.g. 
   
        ln -s /usr/local/dccd-convert-export-<version>/bin/dccd-convert-export /usr/bin



General configuration settings can be set in `cfg/application.properties` and logging can be configured
in `cfg/logback.xml`. The available settings are explained in comments in aforementioned files.


BUILDING FROM SOURCE
--------------------

Prerequisites:

* Java 8 or higher
* Maven 3.3.3 or higher

Steps:

        git clone https://github.com/DANS-KNAW/dccd-convert-export.git
        cd dccd-convert-export
        mvn install
