package jpuzzles;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

//https://leetcode.com/problems/first-missing-positive/
public class FindSmallestPositive {

    public int firstMissingPositive(int[] nums) {
        var uniq = IntStream.of(nums).filter(i -> i > 0 && i <= nums.length).boxed().collect(Collectors.toSet());
        var c = 1;
        while (!uniq.isEmpty()) {
            var removed = uniq.remove(c);
            if (removed) {
                c++;
            } else {
                return c;
            }
        }
        return c;
    }

    public static void main(String[] args) {
        //var res = new FindSmallestPositive().firstMissingPositive(new int[]{1, 2, 0});
        var res = new FindSmallestPositive().firstMissingPositive(new int[]{3, 4, -1, 1});
        System.out.println("res = " + res);
    }
}
