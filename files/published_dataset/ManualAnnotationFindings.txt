Manual Annotation findings:

- "MI:0006", "MI:0007" and "MI:0019" are need to be distinguished from each other. "anti-XXX" for "MI:0006", "tagged" for "MI:0007", "MI:0019" otherwise.

- "MI:0019" ("MI:0006" or "MI:0007"): "immunoblotting" keyword causes to annotation of wrong sentences twith "MI:0019". "Immunoblotting" is "western blotting" actually. Some examples are in article (PMID: 16756390).

    e.g. (PMID: 18547146) 
        <annotation id="3">
            <infon key="type">ExperimentalMethod</infon>
            <infon key="PSIMI">0019</infon>
            <location offset="10533" length="109"/>
            <text>Individual fractions were analyzed by immunoblotting (IB) using Bid, Bax, or Bcl-XL antibodies, as indicated.</text>
        </annotation>

- "MI:0019": "immunopurified", "immunopurification", "copurification" and "colocalization by immunostaining" are all obsolete methods. They cause to annotation of sentences with "MI:0019". They can be removed from word2vec results.

- "MI:0030": "cross-linking" occurs so much in some articles. It can sometimes be used to identify interaction between genes. It needs special focus. Some examples are in article (PMID: 19131970).

- "MI:0049": "western blot" phrase causes to annotation of wrong sentences with "MI:0049". One possible solution would be removing "western blot" and related terms from "MI:0049" word2vec results.

- "MI:0054": Some genetic interactions also can be determined with this method and we need to identify the PPIs from GGIs and only annotate the PPIs related experimental method sentences. Some examples are in article (PMID: 17177603)

- "MI:0052": "fcs" can be removed from synonym list. Its word2vec results cause to annotation of wrong sentences with "MI:0052". Some examples are in article (PMID: 18518979).

- "MI:0065": "NMR titration" is "MI:0065".

- "MI:0077": "NMR titration" phrase causes some problems. Sentences with it needs to be annotated with "MI:0065".

- "MI:0112": Some "two hybrid" related keywords/phrases cause to annotation of wrong sentences with "MI:0112". Some examples are in article (PMID: 18775702).

- "MI:0114": "crystal structure" phrase causes to annotation of wrong sentences with "MI:0114". "Angstrom" or "x-ray" keywords would be helpful to detect correctly.

- "MI:0416": "immunofluorescence" is an experiment done with "fluorescence microscopy", therefore need to be annoated with "MI:0416". Some examples are in article (PMID: 16756390).

- "MI:0419": "gtpase" keyword causes to annotation of wrong sentences with "MI:0419". Word2vec results need to be improved or some results need to be eliminated.

- "MI:0424": "in vitro kinase assay" need to be identified with correct experimental method. Is it "MI:0424" or "MI:0423"? Are "immunocomplex kinase assay", "in vitro immune complex kinase" and "in vitro binding assays" again "MI:0424"?

- "MI:0889": "histone" keyword causes to annotation of wrong sentences with "MI:0889". One possible solution would be removing "histone" and related terms from "MI:0889" word2vec results.

- Some methods are used not to find the interaction. so we should not annotated them.

    e.g. (PMID: 18547146)
        <annotation id="2">
            <infon key="type">ExperimentalMethod</infon>
            <infon key="PSIMI">0071</infon>
            <location offset="10422" length="110"/>
            <text>Membrane-bound proteins were separated from soluble proteins by Sepharose CL-2B gel filtration chromatography.</text>
        </annotation>

- "bret" and "fret" can not precisely be determined in some cases. Some examples are in article (PMID: 16756390).

    e.g.
        <annotation id="8">
            <infon key="type">ExperimentalMethod</infon>
            <infon key="PSIMI">0012</infon> BRET
            <location offset="12217" length="247"/>
            <text>In FRET, a fluorescent donor molecule transfers energy via a nonradiative dipole–dipole interaction to an acceptor molecule [46]. We used a well-known donor: acceptor fluorescent-protein pair, CFP:YFP, with a Förster distance (R 0) of 4.9 nm [47].</text>
        </annotation>

- The passages with only core experimental details have not been annotated. The algorithm needs to pay attention to such situations. Some examples are in article (PMID: 16756390 / offset: 51067 and 53513 passages)

- The algorithm needs to eliminate such situations:

    e.g.

        <passage>
            <infon key="type">paragraph</infon>
            <offset>63016</offset>
            <text>FRET</text>
            <annotation id="38">
                <infon key="type">ExperimentalMethod</infon>
                <infon key="PSIMI">0055</infon>
                <location offset="63016" length="4"/>
                <text>FRET</text>
            </annotation>
        </passage>

        <passage>
            <infon key="type">paragraph</infon>
            <offset>63021</offset>
            <text>fluorescence resonance energy transfer</text>
            <annotation id="39">
                <infon key="type">ExperimentalMethod</infon>
                <infon key="PSIMI">0012</infon>
                <location offset="63021" length="38"/>
                <text>fluorescence resonance energy transfer</text>
            </annotation>
        </passage>