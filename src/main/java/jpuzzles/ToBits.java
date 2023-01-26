package jpuzzles;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ToBits {

    public static void main(String[] args) {
        var nums = IntStream.of(6, 32, 31).boxed().map(n -> String.format("%16s", Integer.toBinaryString(n)).replace(' ', '0')).reduce("", (a, b) -> a + b);
        System.out.println("res: " + nums);
    }
}
