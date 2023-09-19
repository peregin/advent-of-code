package jpuzzles.leetcode;

// https://leetcode.com/problems/palindrome-number/
public class PalindromeNumber {

    public boolean isPalindrome(int x) {
        var s = String.valueOf(x);
        return new StringBuilder(s).reverse().toString().compareTo(s) == 0;
    }

    public static void main(String[] args) {
        var res = new PalindromeNumber().isPalindrome(121);
        System.out.println("res=" + res);
    }
}
