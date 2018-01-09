dccd-convert-export
===========


SYNOPSIS
--------

    dccd-convert-export dccd


DESCRIPTION
-----------

Convert DCCD export to multi-deposit


ARGUMENTS
---------

    Options:
    
          --help      Show help message
          --version   Show version of this program
    
    Subcommand: dccd - Converts Dccd xml files to csv files
          --help   Show help message
    ---
        
EXAMPLES
--------

    dccd-convert-export dccd


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
