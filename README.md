# Latte+ interpreter

Interpreter of a small imperative language with:
* type inference
* nested functions with static binding
* higher order functions, lambdas and closures

Grammar available [here](Latte.cf)

## Examples

```
int max(int a, int b) {
  if (a > b) {
    return a;
  }
  return b;
}

int min(int a, int b) {
  if (a < b) {
    return a;
  }
  return b;
}

int main() {
  int a = 2 + 2 * 2;
  int b = 120 / 9 - 8;
  printInt(a % b);
  printInt(max(a, b));
  printInt(min(a, b));
  printBool(a == a);
  printBool(a == b);
  printBool(true && false);

  return 0;
}
```

```
auto fun(int x) {
    return () => auto {
        int x = 5;
        return x;
    };
}

int main() {
    string a = "a";

    auto g() {
        string b = "b";

        return () => auto {
            string c = "c";

            return () => auto {
                string d = "d";
                
                printString(a);
                printString(b);
                printString(c);
                printString(d);

                return 5;
            };
        };
    };

    auto f = g();
    auto f = f();
    f();

    f = fun(100);
    printInt(f());

    return 0;
}
```
