/*
  2) Таблицы истинности:
  - ((A v В) → (В v С))
  - ¬ А & (B v C)

  Пример логической функции:
    def and(a: Boolean, b: Boolean): Boolean

  Пример функции распечатки таблицы истинности:
    def printTable(f: (Boolean, Boolean, Boolean) => Boolean): Unit

  Пример:
  - A & B & C

  Вызов: printTable((a: Boolean, b: Boolean, c: Boolean) => and(a, and(b, c)))
  Результат:
  A     B     C     Result
  false false false false
  false false true  false
  false true  false false
  false true  true  false
  true  false false false
  true  false true  false
  true  true  false false
  true  true  true  true

  Update - Выполнено отлично. Недочет: лишние кейсы в матчингах.
*/

/*
  v
  Logical disjunction
  T	T	T
  T	F	T
  F	T	T
  F	F	F
*/
def logicalDisjunction(a: Boolean, b: Boolean): Boolean = (a, b) match {
  case (true, _) => true
  case (false, true) => true
  case (false, false) => false
}

/*
  →
  Material conditional
  T	T	T
  T	F	F
  F	T	T
  F	F	T
*/
def materialConditional(a: Boolean, b: Boolean): Boolean = (a, b) match {
  case (true, true) => true
  case (true, false) => false
  case (false, _) => true
}

/*
  ¬
  Negation
  True	False
  False	True
*/
def negation(a: Boolean): Boolean = a match {
  case true => false
  case false => true
}


/*
  &
  Logical conjunction
  T	T	T
  T	F	F
  F	T	F
  F	F	F
*/
def logicalConjunction(a: Boolean, b: Boolean): Boolean = (a, b) match {
  case (true, true) => true
  case (true, false) => false
  case (false, _) => false
}

/*
  &
  Logical conjunction
  F	F	F	F
  F	F	T	F
  F	T	F	F
  F	T	T	T
  T	F	F	F
  T	F	T	F
  T	T	F	F
  T	T	T	T
*/
def logicalConjunction(a: Boolean, b: Boolean, c: Boolean): Boolean = (a, b, c) match {
  case (false, _, _) => false
  case (true, false, _) => false
  case (true, true, false) => false
  case (true, true, true) => true
}

def printTable(f: ((Boolean, Boolean, Boolean) => Boolean)) {
  def print(a: Any, b: Any, c: Any, res: Any) {
    printf("%s	%s	%s	%s	%n", a, b, c, res)
  }
  print("A", "B", "C", "result")
  for (a <- Seq(true, false);
       b <- Seq(true, false);
       c <- Seq(true, false))
    print(a, b, c, f(a, b, c))
}

// A & B & C
printTable((a: Boolean, b: Boolean, c: Boolean) =>
  logicalConjunction(a, b = logicalConjunction(b, c)))

// ((A v В) → (В v С))
printTable((a: Boolean, b: Boolean, c: Boolean) =>
  materialConditional(logicalDisjunction(a, b), logicalDisjunction(b, c)))

// ¬ А & (B v C)
printTable((a: Boolean, b: Boolean, c: Boolean) =>
  logicalConjunction(negation(a), logicalDisjunction(b, c)))