dccd-convert-export
===========


SYNOPSIS
--------

    Usage:


      dccd-convert-export dataPath [<the path of the data>]
      dccd-convert-export dataPath

      Details:

      dccd-convert-export dataPath [<the path of the data>]
       => Use this command to specify a path for the data.

      dccd-convert-export dataPath
       => Use this command to use the default data path.

       Use run.sh instead of dccd-convert-export if you are not on the VM

       The default path is :
       > ./data/projects on your local machine
       > /vagrant/data/projects on the VM

       To use the default path in any case, data must be saved into
       ./data/projects while dccd-convert-export is the current directory



DESCRIPTION
-----------

Convert DCCD export to multi-deposit


ARGUMENTS
---------

    Options:

          --help      Show help message
          --version   Show version of this program

    Subcommand: dataPath - takes dccd data from the given or default path and creates instructions.csv file
          --help   Show help message

     trailing arguments:
      the path of the data (not required)
---
        
EXAMPLES
--------

    dccd-convert-export dccd [<The path of the data>]
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
