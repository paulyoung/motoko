let a : Nat[] = [1, 2, 42];

assert(a.len() == 3);

assert(a[0] == 1);
assert(a[1] == 2);
assert(a[2] == 42);

assert(a.get(0) == 1);
assert(a.get(1) == 2);
assert(a.get(2) == 42);

let b : var Nat[] = [2, 3, 23];

assert(b.len() == 3);

assert(b[0] == 2);
assert(b[1] == 3);
assert(b[2] == 23);

assert(b.get(0) == 2);
assert(b.get(1) == 3);
assert(b.get(2) == 23);

b[1] := 6;
assert(b[1] == 6);

b.set(2, 7);
assert(b[2] == 7);

let oa : {get : Nat -> Nat} = a;
let ob : {set : (Nat, Nat) -> ()} = b;
assert(oa.get(2) == 42);
ob.set(2, 22);

/*
func opt_eq(x : Nat?, y : Nat) : Bool {
  switch x { case null { false };
             case (i?) { i == y } }
};

var it = a.keys();
assert (opt_eq(it.next(), 0));
assert (opt_eq(it.next(), 1));
assert (opt_eq(it.next(), 2));
switch (it.next()) { case null {}; case _ {assert false} };

*/

/*
var i = 0;

i := 0;
for (j in a.keys()) {
  assert(j == i);
  i += 1;
};
assert(i == a.len());

i := 0;
for (n in a.vals()) {
  assert(n == a[i]);
  i += 1;
};
assert(i == a.len());

i := 0;
for (j in b.keys()) {
  assert(j == i);
  i += 1;
};
assert(i == b.len());

i := 0;
for (n in b.vals()) {
  assert(n == b[i]);
  i += 1;
};
assert(i == b.len());
*/
