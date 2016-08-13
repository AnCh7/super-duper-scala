/*
  Используя входную строку "Frank, 123 Main, 925-555-1943,95122" и регулярные выражения,
  получите номер телефона.
  Вы можете конвертировать каждую часть номера телефона к своему собственному целому значению.
  Сохраните результат всех частей выражения в кортеже.
*/

val contacts = "Frank,123 Main,925-555-1943,95122"
val invalidContacts = "Frank,123 Main, 925 - 555 - 1943, 95122"
val phonePattern = """.*,(\d{3})-(\d{3})-(\d{4}),.*""".r

val result = contacts match {
  case phonePattern(part1, part2, part3) => (part1.toInt, part2.toInt, part3.toInt)
}

invalidContacts match {
  case phonePattern(part1, part2, part3) => (part1.toInt, part2.toInt, part3.toInt)
  case _ => println("Do not match")
}