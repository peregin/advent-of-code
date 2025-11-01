package aoc;

import aoc.j2022.Day1;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class Loader {

    public static List<String> input(String resourceName) {
        try (InputStream is = Day1.class.getResourceAsStream(resourceName)) {
            assert is != null;
            BufferedReader br = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8));
            return br.lines().toList();
        } catch (IOException err) {
            System.err.println("unable to read resource " + resourceName);
            return List.of();
        }
    }

    // split by empty lines
    public static List<List<String>> getGroupedLines(String resourceName) {
        var accu = new ArrayList<List<String>>();
        var in = input(resourceName);
        while (!in.isEmpty()) {
            var chunk = in.stream().takeWhile(s -> !s.isEmpty()).toList();
            accu.add(chunk);
            in = in.size() == chunk.size() ? Collections.emptyList() : in.subList(chunk.size() + 1, in.size());
        }
        return accu;
    }
}
