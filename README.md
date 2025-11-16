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
for n in a_list {
    print(n + 1);
}

zero_in_the_list = 0 in the_list;

selector = Select(a_list, the_list, ==);
```
And there is the AST the program would produce:
```
Parsed AST (index-backed):

AssignStmt(name: an_int)
    Int(1)

AssignStmt(name: the_int)
    Int(23)

FnDef(name: add)
    Args:
        Arg: a
        Arg: b
    Body:
        AssignStmt(name: sum)
            Binxpr(+)
                Identifier(a)
                Identifier(b)
        ReturnStmt:
            Identifier(sum)

AssignStmt(name: int_sum)
    FnCall(name: add)
        Args:
            Identifier(a_int)
            Identifier(the_int)

CondExpr
    Then:
        FnCall(name: print)
            Args:
                String("Success")
    If:
        Binxpr(>)
            Identifier(c)
            Int(0)
    Else:
        FnCall(name: print)
            Args:
                Int(0)

CondExpr
    Then:
        Int(0)
    If:
        Binxpr(and)
            Binxpr(-)
                Identifier(an_int)
                Identifier(the_int)
            Binxpr(-)
                Identifier(the_int)
                Identifier(an_int)
    Else:
        Binxpr(or)
            Identifier(int_sum)
            String("Huh?")

AssignStmt(name: a_list)
    List
        Int(1)
        Int(2)
        Int(3)

AssignStmt(name: a_dict)
    Dictionary
        Pair
            String("integer")
            Int(1)
        Pair
            String("list")
            List
                Int(2)
                Int(3)

AssignStmt(name: the_list)
    List
        Int(0)
        String("one")
        Int(1)

AssignStmt(name: zero)
    IndexExpr
        Target:
            Identifier(the_list)
        Index:
            IndexExpr
                Target:
                    Identifier(a_list)
                Index:
                    Int(0)

ForStmt(var: n)
    Iterable:
        Identifier(a_list)
    Body:
        Binxpr(+)
            Identifier(n)
            Int(1)

AssignStmt(name: zero_in_the_list)
    Binxpr(in)
        Int(0)
        Identifier(the_list)

AssignStmt(name: selector)
    FnCall(name: Select)
        Args:
            Identifier(a_list)
            Identifier(the_list)
            SelectorPred(==)
```
