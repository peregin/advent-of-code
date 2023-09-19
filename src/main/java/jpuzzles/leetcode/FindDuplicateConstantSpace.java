package jpuzzles.leetcode;

import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

// https://leetcode.com/problems/find-the-duplicate-number/
public class FindDuplicateConstantSpace {

    public int findDuplicate(int[] nums) {
        var res = IntStream.of(nums).boxed()
                .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()))
                .entrySet().stream().filter(e -> e.getValue() > 1).map(e -> e.getKey())
                .mapToInt(Integer::intValue).findFirst().orElseGet(() -> -1);
        //System.out.println(res);
        return res;
    }

    public int findDuplicate2(int[] nums) {
        // precisely one integer can appear multiple times [2, 2, 2, 2, 2]
        var first = nums[0];
        var theSame = IntStream.of(nums).allMatch(i -> i == first);
        if (theSame) {
            return first;
        } else {
            var n = nums.length;
            var series = (n * (n - 1)) / 2;
            var sum = IntStream.of(nums).sum();
            return sum - series;
        }
    }

    // only one repetead number
    // n + 1 integers
    // range is [1, n]
    // formula sum = n * (n + 1) / 2
    // sum 3 = 3 * 4 / 2 = 6
    public static void main(String[] args) {
        //var res = new FindDuplicateConstantSpace().findDuplicate(new int[]{1, 3, 4, 2, 2});
        //var res = new FindDuplicateConstantSpace().findDuplicate(new int[]{3, 1, 3, 4, 2});
        //var res = new FindDuplicateConstantSpace().findDuplicate(new int[]{2, 2, 2, 2, 2});
        var res = new FindDuplicateConstantSpace().findDuplicate(new int[]{1, 4, 4, 2, 4});
        System.out.println("res=" + res);
    }
}
