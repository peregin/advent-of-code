package jpuzzles;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class RemoveDuplicates2 {

    public static int removeDuplicates1(ArrayList<Integer> a) {
        int count = 0;
        Optional<Integer> prev = Optional.empty();
        int cur = 0;
        for (Integer i: a) {
            if (prev.map(p -> p.compareTo(i) == 0).orElse(false)) {
                cur = 2;
            } else {
                if (prev.isPresent()) count += cur;
                cur = 1;
            }
            prev = Optional.of(i);
        }
        count += cur;
        return count;
    }

    public static int removeDuplicates(ArrayList<Integer> a) {
        Map<Integer, List<Integer>> map = a.stream().collect(Collectors.groupingBy(i -> i));
        System.out.println(map);
        return map.values().stream().map(list -> Math.min(list.size(), 2)).mapToInt(Integer::intValue).sum();
    }

    public static void main(String[] args) {
        int res = removeDuplicates(new ArrayList(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)));
        System.out.println("res is " + res);
    }
}
