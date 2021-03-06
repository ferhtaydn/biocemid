#### BioC Experimental Molecular Interaction Detection

[BioCreative V - BioC Track (I) - Task 6 Implementation](http://www.biocreative.org/tasks/biocreative-v/track-1-bioc/)

- The code is under [source](src/main/scala/com/ferhtaydn/biocemid/) directory. 
- Target experimental methods from PSI-MI ontology are in [methods file](src/main/resources/methods.conf).
- The [main](src/main/scala/com/ferhtaydn/biocemid/Main.scala) class is `com.ferhtaydn.biocemid.Main`.
You may need to modify annotation and evaluation configurations in order to obtain your desired operation.
- All the resources related to word2vecs, raw and annotated articles are located under [files](files/) directory.
- The published data set, consists of 30 articles, is located in [published dataset](files/published_dataset/) directory.

The versions of the libraries which are used:  
* [Java Version: 1.8.0_91](http://download.oracle.com/otn-pub/java/jdk/8u91-b14/jdk-8u91-macosx-x64.dmg)
* [Scala Version: 2.11.8](http://search.maven.org/#search|ga|1|g%3A%22org.scala-lang%22%20AND%20v%3A%222.11.8%22)
* [Sbt Version: 0.13.11](https://dl.bintray.com/sbt/native-packages/sbt/0.13.11/sbt-0.13.11.zip)
* [Scala Xml Version: 1.0.5](http://search.maven.org/#artifactdetails|org.scala-lang.modules|scala-xml_2.11|1.0.5|bundle)
* [Typesafe Config Version: 1.3.0](http://search.maven.org/#artifactdetails|com.typesafe|config|1.3.0|)
* [Stanford CoreNLP Version: 3.6.0](http://nlp.stanford.edu/software/stanford-corenlp-full-2015-12-09.zip)
* [BioC_Java Version: 1.0.1](https://sourceforge.net/projects/bioc/files/BioC_Java_1.0.1.tar.gz/download)
* [Word2vec Version: Revision 42](http://word2vec.googlecode.com/svn/trunk/)

- Working environment:
  - MacBook Pro (Early 2015)
  - OS X El Capitan Version 10.11.4
  - 2.7 GHz Intel Core i5
  - 8 GB 1867 MHz DDR3
  - 128 GB Flash Storage

- Word2vec is run on a cluster machine which has:
  - Ubuntu 14.04.4 LTS
  - Kernel: 3.19.0-43-generic
  - GCC: version 4.8.4 (Ubuntu 4.8.4-2ubuntu1~14.04.1)
  - Ram: 64GB
  - CPU:
    - Intel(R) Xeon(R) CPU E5410 @ 2.33GHz
    - Architecture:          x86_64
    - CPU op-mode(s):        32-bit, 64-bit
    - Byte Order:            Little Endian
    - CPU(s):                8
    - On-line CPU(s) list:   0-7
    - Thread(s) per core:    1
    - Core(s) per socket:    4
    - Socket(s):             2
    - NUMA node(s):          1
    - Vendor ID:             GenuineIntel
    - CPU family:            6
    - Model:                 23
    - Stepping:              6
    - CPU MHz:               2327.326
    - BogoMIPS:              4655.07
    - Virtualization:        VT-x
    - L1d cache:             32K
    - L1i cache:             32K
    - L2 cache:              6144K
    - NUMA node0 CPU(s):     0-7