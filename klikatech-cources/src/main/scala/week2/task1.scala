package week2

/*
1. Реализовать классы:
а) case class I(i: Int) // Int
б) case class D(d: Double) // Double
- с методами (операциями)  + - * /  (по желанию, прочие арифметические операции),
рассматриваемый пример для ознакомления: klikatech._2_week._6_left_assoc.MyInt

Комментарий к задаче:
- Обращайте внимание на приоритеты операций и ассоциативность операций (очередность операций при равном приоритете)
- Использовать двоеточие ':' в имени метода операции, например:
+: или :+ или :+: - различные варианты.
Проверить очередность вычисления выражения в таком случае.
Ответить на вопрос, в чем разница между операциями  +:  и  *:
 */

case class NewInteger(i: Int) {

  def +(that: NewInteger): NewInteger = NewInteger(this.i + that.i)
  def -(that: NewInteger): NewInteger = NewInteger(this.i - that.i)
  def *(that: NewInteger): NewInteger = NewInteger(this.i * that.i)
  def /(that: NewInteger): NewInteger = NewInteger(this.i / that.i)
  def %(that: NewInteger): NewInteger = NewInteger(this.i % that.i)
  def ++ : NewInteger = NewInteger(this.i + 1)
  def -- : NewInteger = NewInteger(this.i - 1)

  def +:(that: NewInteger): NewInteger = NewInteger(this.i + that.i)
  def -:(that: NewInteger): NewInteger = NewInteger(this.i - that.i)
  def *:(that: NewInteger): NewInteger = NewInteger(this.i * that.i)
  def /:(that: NewInteger): NewInteger = NewInteger(this.i / that.i)
  def %:(that: NewInteger): NewInteger = NewInteger(this.i % that.i)
  def ++: : NewInteger = NewInteger(this.i + 1)
  def --: : NewInteger = NewInteger(this.i - 1)

  def :+(that: NewInteger): NewInteger = NewInteger(this.i + that.i)
  def :-(that: NewInteger): NewInteger = NewInteger(this.i - that.i)
  def :*(that: NewInteger): NewInteger = NewInteger(this.i * that.i)
  def :/(that: NewInteger): NewInteger = NewInteger(this.i / that.i)
  def :%(that: NewInteger): NewInteger = NewInteger(this.i % that.i)
  def :++ : NewInteger = NewInteger(this.i + 1)
  def :-- : NewInteger = NewInteger(this.i - 1)

  def :+:(that: NewInteger): NewInteger = NewInteger(this.i + that.i)
  def :-:(that: NewInteger): NewInteger = NewInteger(this.i - that.i)
  def :*:(that: NewInteger): NewInteger = NewInteger(this.i * that.i)
  def :/:(that: NewInteger): NewInteger = NewInteger(this.i / that.i)
  def :%:(that: NewInteger): NewInteger = NewInteger(this.i % that.i)
  def :++: : NewInteger = NewInteger(this.i + 1)
  def :--: : NewInteger = NewInteger(this.i - 1)
}

object IntApp extends App {

  println("===================== Left-associative...")

  var x = NewInteger(1) + NewInteger(2) * NewInteger(3)
  assert(x.i == 7)
  println(s"1 + 2 * 3 = $x")

  x = NewInteger(1) + (NewInteger(2) * NewInteger(3))
  assert(x.i == 7)
  println(s"1 + (2 * 3) = $x")

  x = (NewInteger(1) + NewInteger(2)) * NewInteger(3)
  assert(x.i == 9)
  println(s"(1 + 2) * 3 = $x")

  x = NewInteger(1).+(NewInteger(2)).*(NewInteger(3))
  assert(x.i == 9)
  println(s"1 + 2 * 3 = $x")

  x = NewInteger(2) - NewInteger(2)
  assert(x.i == 0)
  println(s"2 - 2 = $x")

  x = NewInteger(9) / NewInteger(3)
  assert(x.i == 3)
  println(s"9 / 3 = $x")

  x = NewInteger(2) % NewInteger(2)
  assert(x.i == 0)
  println(s"2 % 2 = $x")

  x = NewInteger(10).++
  assert(x.i == 11)
  println(s"10++ = $x")

  x = NewInteger(10)
  assert((x--).i == 9)
  println(s"10-- = $x")

  println("===================== Right-associative...")

  x = NewInteger(4) -: NewInteger(2) +: NewInteger(3)
  assert(x.i == 1)
  println(s"4 -: 2 +: 3 = $x")

  x = NewInteger(4) -: (NewInteger(2) +: NewInteger(3))
  assert(x.i == 1)
  println(s"4 -: (2 +: 3) = $x")

  x = (NewInteger(4) -: NewInteger(2)) +: NewInteger(3)
  assert(x.i == 1)
  println(s"(4 -: 2) +: 3 = $x")

  x = NewInteger(4).-:(NewInteger(2)).+:(NewInteger(3))
  assert(x.i == 5)
  println(s"4 -: 2 +: 3 = $x")

  x = NewInteger(3) -: NewInteger(2)
  assert(x.i == -1)
  println(s"3 -: 2 = $x")

  x = NewInteger(3) /: NewInteger(9)
  assert(x.i == 3)
  println(s"3 /: 9 = $x")

  x = NewInteger(2) %: NewInteger(2)
  assert(x.i == 0)
  println(s"2 %: 2 = $x")

  x = NewInteger(10).++:
  assert(x.i == 11)
  println(s"10++: = $x")

  x = NewInteger(10)
  assert((x--:).i == 9)
  println(s"10--: = $x")

  println("===================== Left-associative...")

  x = NewInteger(4) :- NewInteger(2) :+ NewInteger(3)
  assert(x.i == 5)
  println(s"4 :- 2 :+ 3 = $x")

  x = NewInteger(4) :- (NewInteger(2) :+ NewInteger(3))
  assert(x.i == -1)
  println(s"4 :- (2 :+ 3) = $x")

  x = (NewInteger(4) :- NewInteger(2)) :+ NewInteger(3)
  assert(x.i == 5)
  println(s"(4 :- 2) :+ 3 = $x")

  x = NewInteger(4).:-(NewInteger(2)).:+(NewInteger(3))
  assert(x.i == 5)
  println(s"4 :- 2 :+ 3 = $x")

  x = NewInteger(3) :- NewInteger(2)
  assert(x.i == 1)
  println(s"3 :- 2 = $x")

  x = NewInteger(3) :/ NewInteger(9)
  assert(x.i == 0)
  println(s"3 :/ 9 = $x")

  x = NewInteger(2) :% NewInteger(2)
  assert(x.i == 0)
  println(s"2 :% 2 = $x")

  x = NewInteger(10).:++
  assert(x.i == 11)
  println(s"10 :++ = $x")

  x = NewInteger(10)
  assert((x:--).i == 9)
  println(s"10 :-- = $x")

  println("===================== Right-associative...")

  x = NewInteger(4) :-: NewInteger(2) :+: NewInteger(3)
  assert(x.i == 1)
  println(s"4 :-: 2 :+: 3 = $x")

  x = NewInteger(4) :-: (NewInteger(2) :+: NewInteger(3))
  assert(x.i == 1)
  println(s"4 :-: (2 :+: 3) = $x")

  x = (NewInteger(4) :-: NewInteger(2)) :+: NewInteger(3)
  assert(x.i == 1)
  println(s"(4 :-: 2) :+: 3 = $x")

  x = NewInteger(4).:-:(NewInteger(2)).:+:(NewInteger(3))
  assert(x.i == 5)
  println(s"4 :-: 2 :+: 3 = $x")

  x = NewInteger(3) :-: NewInteger(2)
  assert(x.i == -1)
  println(s"3 :-: 2 = $x")

  x = NewInteger(3) :/: NewInteger(9)
  assert(x.i == 3)
  println(s"3 :/: 9 = $x")

  x = NewInteger(2) :%: NewInteger(2)
  assert(x.i == 0)
  println(s"2 :%: 2 = $x")

  x = NewInteger(10).:++:
  assert(x.i == 11)
  println(s"10 :++: = $x")

  x = NewInteger(10)
  assert((x:--:).i == 9)
  println(s"10 :--: = $x")
}

case class NewDouble(d: Double) {

  def +(that: NewDouble): NewDouble = NewDouble(this.d + that.d)
  def -(that: NewDouble): NewDouble = NewDouble(this.d - that.d)
  def *(that: NewDouble): NewDouble = NewDouble(this.d * that.d)
  def /(that: NewDouble): NewDouble = NewDouble(this.d / that.d)
  def %(that: NewDouble): NewDouble = NewDouble(this.d % that.d)
  def ++ : NewDouble = NewDouble(this.d + 1)
  def -- : NewDouble = NewDouble(this.d - 1)

  def +:(that: NewDouble): NewDouble = NewDouble(this.d + that.d)
  def -:(that: NewDouble): NewDouble = NewDouble(this.d - that.d)
  def *:(that: NewDouble): NewDouble = NewDouble(this.d * that.d)
  def /:(that: NewDouble): NewDouble = NewDouble(this.d / that.d)
  def %:(that: NewDouble): NewDouble = NewDouble(this.d % that.d)
  def ++: : NewDouble = NewDouble(this.d + 1)
  def --: : NewDouble = NewDouble(this.d - 1)

  def :+(that: NewDouble): NewDouble = NewDouble(this.d + that.d)
  def :-(that: NewDouble): NewDouble = NewDouble(this.d - that.d)
  def :*(that: NewDouble): NewDouble = NewDouble(this.d * that.d)
  def :/(that: NewDouble): NewDouble = NewDouble(this.d / that.d)
  def :%(that: NewDouble): NewDouble = NewDouble(this.d % that.d)
  def :++ : NewDouble = NewDouble(this.d + 1)
  def :-- : NewDouble = NewDouble(this.d - 1)

  def :+:(that: NewDouble): NewDouble = NewDouble(this.d + that.d)
  def :-:(that: NewDouble): NewDouble = NewDouble(this.d - that.d)
  def :*:(that: NewDouble): NewDouble = NewDouble(this.d * that.d)
  def :/:(that: NewDouble): NewDouble = NewDouble(this.d / that.d)
  def :%:(that: NewDouble): NewDouble = NewDouble(this.d % that.d)
  def :++: : NewDouble = NewDouble(this.d + 1)
  def :--: : NewDouble = NewDouble(this.d - 1)
}

object DoubleApp extends App {

  println("===================== Left-associative...")

  var x = NewDouble(1.0) + NewDouble(2.0) * NewDouble(3.0)
  assert(x.d == 7.0)
  println(s"1.0 + 2.0 * 3.0 = $x")

  x = NewDouble(1.0) + (NewDouble(2.0) * NewDouble(3.0))
  assert(x.d == 7.0)
  println(s"1.0 + (2.0 * 3.0) = $x")

  x = (NewDouble(1.0) + NewDouble(2.0)) * NewDouble(3.0)
  assert(x.d == 9.0)
  println(s"(1.0 + 2.0) * 3.0 = $x")

  x = NewDouble(1.0).+(NewDouble(2.0)).*(NewDouble(3.0))
  assert(x.d == 9.0)
  println(s"1.0 + 2.0 * 3.0 = $x")

  x = NewDouble(2.0) - NewDouble(2.0)
  assert(x.d == 0.0)
  println(s"2.0 - 2.0 = $x")

  x = NewDouble(9.0) / NewDouble(3.0)
  assert(x.d == 3.0)
  println(s"9.0 / 3.0 = $x")

  x = NewDouble(2.0) % NewDouble(2.0)
  assert(x.d == 0.0)
  println(s"2.0 % 2.0 = $x")

  x = NewDouble(10.0).++
  assert(x.d == 11.0)
  println(s"10.0++ = $x")

  x = NewDouble(10.0)
  assert((x--).d == 9.0)
  println(s"10.0-- = $x")

  println("===================== Right-associative...")

  x = NewDouble(4.0) -: NewDouble(2.0) +: NewDouble(3.0)
  assert(x.d == 1.0)
  println(s"4.0 -: 2.0 +: 3.0 = $x")

  x = NewDouble(4.0) -: (NewDouble(2.0) +: NewDouble(3.0))
  assert(x.d == 1.0)
  println(s"4.0 -: (2.0 +: 3.0) = $x")

  x = (NewDouble(4.0) -: NewDouble(2.0)) +: NewDouble(3.0)
  assert(x.d == 1.0)
  println(s"(4.0 -: 2.0) +: 3.0 = $x")

  x = NewDouble(4.0).-:(NewDouble(2.0)).+:(NewDouble(3.0))
  assert(x.d == 5.0)
  println(s"4.0 -: 2.0 +: 3.0 = $x")

  x = NewDouble(3.0) -: NewDouble(2.0)
  assert(x.d == -1.0)
  println(s"3.0 -: 2.0 = $x")

  x = NewDouble(3.0) /: NewDouble(9.0)
  assert(x.d == 3.0)
  println(s"3.0 /: 9.0 = $x")

  x = NewDouble(2.0) %: NewDouble(2.0)
  assert(x.d == 0.0)
  println(s"2.0 %: 2.0 = $x")

  x = NewDouble(10.0).++:
  assert(x.d == 11.0)
  println(s"10.0++: = $x")

  x = NewDouble(10.0)
  assert((x--:).d == 9.0)
  println(s"10.0--: = $x")

  println("===================== Left-associative...")

  x = NewDouble(4.0) :- NewDouble(2.0) :+ NewDouble(3.0)
  assert(x.d == 5.0)
  println(s"4.0 :- 2.0 :+ 3.0 = $x")

  x = NewDouble(4.0) :- (NewDouble(2.0) :+ NewDouble(3.0))
  assert(x.d == -1.0)
  println(s"4.0 :- (2.0 :+ 3.0) = $x")

  x = (NewDouble(4.0) :- NewDouble(2.0)) :+ NewDouble(3.0)
  assert(x.d == 5.0)
  println(s"(4.0 :- 2.0) :+ 3.0 = $x")

  x = NewDouble(4.0).:-(NewDouble(2.0)).:+(NewDouble(3.0))
  assert(x.d == 5.0)
  println(s"4.0 :- 2.0 :+ 3.0 = $x")

  x = NewDouble(3.0) :- NewDouble(2.0)
  assert(x.d == 1.0)
  println(s"3.0 :- 2.0 = $x")

  x = NewDouble(9.0) :/ NewDouble(3.0)
  assert(x.d == 3.0)
  println(s"9.0 :/ 3.0 = $x")

  x = NewDouble(2.0) :% NewDouble(2.0)
  assert(x.d == 0.0)
  println(s"2.0 :% 2.0 = $x")

  x = NewDouble(10.0).:++
  assert(x.d == 11.0)
  println(s"10.0 :++ = $x")

  x = NewDouble(10.0)
  assert((x:--).d == 9.0)
  println(s"10.0 :-- = $x")

  println("===================== Right-associative...")

  x = NewDouble(4.0) :-: NewDouble(2.0) :+: NewDouble(3.0)
  assert(x.d == 1.0)
  println(s"4.0 :-: 2.0 :+: 3.0 = $x")

  x = NewDouble(4.0) :-: (NewDouble(2.0) :+: NewDouble(3.0))
  assert(x.d == 1.0)
  println(s"4.0 :-: (2.0 :+: 3.0) = $x")

  x = (NewDouble(4.0) :-: NewDouble(2.0)) :+: NewDouble(3.0)
  assert(x.d == 1.0)
  println(s"(4.0 :-: 2.0) :+: 3.0 = $x")

  x = NewDouble(4.0).:-:(NewDouble(2.0)).:+:(NewDouble(3.0))
  assert(x.d == 5.0)
  println(s"4.0 :-: 2.0 :+: 3.0 = $x")

  x = NewDouble(3.0) :-: NewDouble(2.0)
  assert(x.d == -1.0)
  println(s"3.0 :-: 2.0 = $x")

  x = NewDouble(3.0) :/: NewDouble(9.0)
  assert(x.d == 3.0)
  println(s"3.0 :/: 9.0.0 = $x")

  x = NewDouble(2.0) :%: NewDouble(2.0)
  assert(x.d == 0.0)
  println(s"2.0 :%: 2.0 = $x")

  x = NewDouble(10.0).:++:
  assert(x.d == 11.0)
  println(s"10.0 :++: = $x")

  x = NewDouble(10.0)
  assert((x:--:).d == 9.0)
  println(s"10.0 :--: = $x")
}