# ⚙️ RVMZ

RVMZ (RASP Virtual Machine in Zig) is a virtual machine designed to interpret
the Restricted Access Sequence Processing language.

## 🚧 Project Status

RVMZ is a work-in-progress project. For now, it only builds an
AST for the program.
+ The next stage would be building a tree-walking interpreter (because it is
  more fast and straight-forward process).
+ The stage after that would be replacing the tree-walking interperter with a
  register-based virtual machine to gain speed.
+ The last stage is fully dedicated to wrtiting down the specification.

## ⌨️ Example

Look into the main.zig. Here is a RASP program stored in the `source` variable:
```python
an_int = 1;
the_int = 23;


def add(a, b) {
    sum = a + b;
    return sum;
}


int_sum = add(a_int, the_int);

print("Success") if c > 0 else print(0);
0 if an_int - the_int and the_int - an_int else int_sum or "Huh?";

a_list = [1, 2, 3];
a_dict = {"integer": 1, "list": [2, 3]};
the_list = [0, {"one": 1}, 2 + 3];

zero = the_list[a_list[0]];
```
