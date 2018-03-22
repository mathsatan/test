import java.util.LinkedList;
import java.util.List;

public class Dioph {
    public static void go() {
        List<Integer> list = new LinkedList<>(); list.add(3); list.add(3); list.add(4);
        List<Integer> list1 = new LinkedList<>(); //list1.add(1);
        List<Integer> list2 = new LinkedList<>(); //list2.add(9); list2.add(9);
        Tuple<List<Integer>, List<Integer>> tuple = new Tuple<>(list1, list2);
        printListOfTuples(comb(list, tuple));
    }

    public static List<Integer> factorize(Integer _x) {
        List<Integer> result = new LinkedList<>();
        Integer x = _x;
        Integer a = 2;
        for (;;) {
            boolean b = a * a > x;
            if (!b && x % a == 0) {
                result.add(a);
                x = x / a;
            } else if (b) {
                result.add(x);
                break;
            } else {
                a += 1;
            }
        }
        return result;
    }

    private static List<Tuple<List<Integer>, List<Integer>>> g(List<Integer> list, Tuple<List<Integer>, List<Integer>> tuple) {
        List<Tuple<List<Integer>, List<Integer>>> result = new LinkedList<>();
        if (list != null && tuple != null) {
            for (int i = 0; i <= list.size(); ++i) {
                List<Integer> list1 = new LinkedList<>();
                list1.addAll(tuple._1());
                list1.addAll(list.subList(0, i));

                List<Integer> list2 = new LinkedList<>();
                list2.addAll(tuple._2());
                list2.addAll(list.subList(i, list.size()));

                Tuple<List<Integer>, List<Integer>> x = new Tuple<>(list1, list2);
                result.add(x);
            }
        }
        return result;
    }

    public static void printListOfTuples(List<Tuple<List<Integer>, List<Integer>>> list) {
        String str = "";
        for (Tuple<List<Integer>, List<Integer>> tuple : list) {
            str += "(" + makeString(tuple._1(), "*") + "; " + makeString(tuple._2(), "*") + ")\n";
        }
        System.out.print(str);
    }

    private static String makeString(List<Integer> list, String delimiter) {
        if (list.size() == 0) {
            return " - ";
        }
        StringBuilder listString = new StringBuilder();
        int j = 0;
        for (Integer i : list) {
            listString.append(i);
            if (j < list.size() - 1) {
                listString.append(delimiter);
            }
            ++j;
        }
        return listString.toString();
    }

    private static List<Tuple<List<Integer>, List<Integer>>>
        concat(List<Tuple<List<Integer>, List<Integer>>> list1,
               List<Tuple<List<Integer>, List<Integer>>> list2) {

        List<Tuple<List<Integer>, List<Integer>>> r = new LinkedList<>();
        if (list1 != null) {
            r.addAll(list1);
        }
        if (list2 != null) {
            r.addAll(list2);
        }
        return r;
    }

    private static List<Integer> addToHead(Integer a, List<Integer> list) {
        List<Integer> r = new LinkedList<>();
        if (list != null) {
            r.add(a);
            r.addAll(list);
        }
        return r;
    }

    public static List<Tuple<List<Integer>, List<Integer>>>
        comb(List<Integer> list, Tuple<List<Integer>, List<Integer>> a) {
        if (list.size() >= 2) {
            Integer h = list.get(0);
            List<Integer> t = list.subList(1, list.size());

            int power = getPower(list, h);
            if (power < 2) {
                Tuple<List<Integer>, List<Integer>> u = new Tuple<>(addToHead(h, a._1()), a._2());
                Tuple<List<Integer>, List<Integer>> v = new Tuple<>(a._1(), addToHead(h, a._2()));
                return concat(comb(t, u), comb(t, v));
            } else {
                List<Integer> l = new LinkedList<>();
                for (int i = 0; i < power; ++i) l.add(h);
                return g(l, a);
            }
        } else {
            List<Tuple<List<Integer>, List<Integer>>> result = new LinkedList<>();
            if (list.size() == 1) {
                Tuple<List<Integer>, List<Integer>> a1 = new Tuple<>(addToHead(list.get(0), a._1()), a._2());
                Tuple<List<Integer>, List<Integer>> a2 = new Tuple<>(a._1(), addToHead(list.get(0), a._2()));
                result.add(a1);
                result.add(a2);
            } else {
                result.add(a);
            }
            return result;
        }
    }

    private static int getPower(List<Integer> list, Integer a) {
        int p = 0;
        if (list != null) {
            for (Integer i: list) {
                if (i.equals(a)) {
                    ++p;
                }
            }
        }
        return p;
    }
}
