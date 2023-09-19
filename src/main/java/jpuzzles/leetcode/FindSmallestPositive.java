package jpuzzles.leetcode;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

//https://leetcode.com/problems/first-missing-positive/
public class FindSmallestPositive {

    public int firstMissingPositive(int[] nums) {
        // cyclic sort on nums
        var n = nums.length;
        for (int i = 0; i < n; i++) {
            var v = nums[i];
            if (v >= 0 && v < n) {
                var w = nums[v];
                if (w >= 0 && w < n && v != w)
                nums[i] = w;
                nums[v] = v;
            }
        }
        System.out.println(IntStream.of(nums).boxed().toList());


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
