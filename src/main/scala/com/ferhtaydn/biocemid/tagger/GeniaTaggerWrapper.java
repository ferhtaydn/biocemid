package com.ferhtaydn.biocemid.tagger;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.List;

/**
 * https://github.com/syeedibnfaiz/NLP-Lib/blob/master/src/ca/uwo/csd/ai/nlp/utils/GeniaTaggerWrapper.java
 */
public class GeniaTaggerWrapper {

    private String path2GeniaTagger;
    private boolean tokenize;
    private Process p;
    private boolean initialized;
    private BufferedReader reader;
    private BufferedWriter writer;
    private boolean ascii = true;

    public GeniaTaggerWrapper() {
        this(System.getProperty("user.dir") + "/lib/geniatagger-3.0.2/geniatagger", false);
    }

    public GeniaTaggerWrapper(String path2GeniaTagger, boolean tokenize) {
        this.path2GeniaTagger = path2GeniaTagger;
        this.tokenize = tokenize;
        initialized = init();
    }

    private boolean init() {
        String workingDir = "./";

        if (path2GeniaTagger.contains("/")) {
            int where = path2GeniaTagger.lastIndexOf("/");
            workingDir = path2GeniaTagger.substring(0, where);
        }

        try {
            File file = new File(workingDir);
            if (!file.exists()) System.out.println(workingDir + " does not exist.");
            file = new File(path2GeniaTagger);
            if (!file.exists()) System.out.println(path2GeniaTagger + " does not exist.");

            if (tokenize) p = Runtime.getRuntime().exec(path2GeniaTagger, null, new File(workingDir));
            else p = Runtime.getRuntime().exec(new String[]{path2GeniaTagger, "-nt"}, null, new File(workingDir));

            reader = new BufferedReader(new InputStreamReader(p.getInputStream(), "US-ASCII"));
            writer = new BufferedWriter(new OutputStreamWriter(p.getOutputStream(), "US-ASCII"));

            System.out.println("Genia tagger is now loading...");
            Thread.sleep(15000);
            System.out.println("Loading done.");

        } catch (Exception ex) {
            System.err.println(ex);
            return false;
        }
        return true;
    }

    public String[][] doTagging(List<String> tokens) {
        if (!initialized) {
            System.err.println("Not initialized");
            return null;
        }
        try {
            String input = "";
            for (int i = 0; i < tokens.size(); i++) {
                if (i != 0) input += " ";
                input += tokens.get(i);
            }
            input += "\n";

            if (input.length() >= 1024) throw new IllegalArgumentException("Input is too large for genia tagger.");
            if (ascii) input = convert2ASCII(input);

            writer.write(input);
            writer.flush();

            String output[][] = new String[tokens.size()][];
            String line;
            int i = 0;
            while ((line = reader.readLine()) != null) {
                if (line.equals("")) break;
                output[i++] = line.split("\\s+");
            }

            return output;
        } catch (IOException ex) {
            System.out.println(ex);
            return null;
        }
    }

    private String convert2ASCII(String s) {
        String tmp = "";
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) > 128) tmp += '-';
            else tmp += s.charAt(i);
        }
        return tmp;
    }
}