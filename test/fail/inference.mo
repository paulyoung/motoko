func id<T>(x:T):T { x };
ignore id(1);
ignore id<Nat>(1);

func swap<T,U>(x:T,y:U):(U,T){(y,x)};
ignore swap(1,true);
ignore swap(true,1);

type N = <T>(T->T,T)->T;

func zero<T>(f:T->T,z:T):T{z};

func succ(n:N):N {
  func <T>(f:T->T,z:T) : T { f(n/*<T>*/(f,z))}
};

ignore zero;
ignore succ(zero);
ignore succ(succ zero);

func app<T,U>(f: T-> U, x:T) : U { f(x); };

ignore app/*<N,N>*/(succ,zero);

func compose<T,U,V>(f: T -> U, g : U-> V) : T->V { func x { g (f(x))} };

ignore compose((func(x:Int):Int=x),(func(x:Int):Int=x));
ignore compose/*<N,N,N>*/(succ,succ);



type List<T> = ?(T,List<T>);
type List2<T> = ?(T,List2<T>);

func cons<T>(h:T,t:List<T>):List<T> {?(h,t)};
func nil<T>():List<T>{ null };

ignore nil<Nat>();
ignore cons(1,nil<Nat>());
ignore cons(1,cons(2,nil<Nat>()));
ignore cons(1,cons(2,null:List<Nat>));
ignore cons(1,cons(2,null:List2<Nat>:List<Nat>));
ignore cons(1,cons(2,null:List2<Nat>));

func curry<A,B,C>(f : (A,B)->C) : A->B->C {
  func(a:A): B->C { func(b:B):C { f(a,b) } }
};

func add(m:N,n:N):N { m/*<N>*/(succ,n) };

func mult1(m:N,n:N):N { m/*<N>*/(func (a:N) : N { add(a,n)},zero) };

func mult2(m:N,n:N):N { m/*<N>*/(curry/*<N,N,N>*/ add n, zero) };


func tricky<T>(f : T -> T) { };

tricky<None>(func f(x:Any):None { f(x);});
tricky<Any>(func f(x:Any):None { f(x);});
ignore func <T>()  { tricky<T>(func f(x:Any):None{f(x)}) };

ignore tricky(func f(x:Any):None { f(x);});
ignore tricky(func f(x:None):Any { f(x);}); //requires Dolan style bi-matching
ignore tricky(func f(x:None):None { f(x);}); //requires Dolan style bi-matching

func amb<T>(f : T -> T): T->T { f };
ignore amb<None>(func f(x:Any):None { f(x);}) : None -> None;
ignore amb<Any>(func f(x:Any):None { f(x);}) : Any -> Any;


func co<T>(x : T,y : T):(){};
ignore co<Nat>(1,2);
ignore co<Int>(1,2:Int);
ignore co<Any>(1,true);
ignore co(1,2);
ignore co(1,2:Int);
ignore co(1,true);


func contra<T>(f:(T,T) -> ()):(){};
ignore contra<Nat>(func (x:Nat,y:Nat) {});
ignore contra<Nat>(func (x:Nat,y:Int) {});
ignore contra<None>(func (x:Nat,y:Bool) {});
ignore contra(func (x:Nat,y:Nat) {});
ignore contra(func (x:Nat,y:Int) {});
ignore contra(func (x:Nat,y:Bool) {});

/*
func coswap<T <: U,U>(x : T,y : T):(U,U){(y,x)};
ignore coswap(1,2); // works (coz Int <: Nat)
ignore coswap(1,2:Int); // works (coz Int <: Nat)
ignore coswap(1,true); // doesn't work unless we lub with with bound
*/

// support domain driven overloading for scoped monomorphic functions (as before)
func f(g:shared Nat8 -> ()) : async () {
  g(1);
};

func bnd<T <: Int>(x:T):T{x};
ignore bnd(1:Int) : Int;
ignore bnd(1) : Nat;
ignore (if false (bnd(loop {}):Nat) else 1);
ignore (if false (bnd(loop {}):Int) else 1);
ignore (if false (bnd(loop {}):None) else 1);
bnd(true);


