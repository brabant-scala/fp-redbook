package nl.hugo.redbook.ch3

object Exercise15 {
  import nl.hugo.redbook.ch3.List._
  def flatAppend[A](ls: List[List[A]]): List[A] =
    /* loop door elke list
     nu heb ik een List[List[A]] en een List[A]
     hiermee moet ik de die list[A] concatten met resultaat van andere
     en over die List[List[A]] recursief flatappend aanroepen

     haal individulele elementen daaruit

     geef die terug als list

     en voeg toe aan resultaat
    */
    foldLeft(ls, List[A]())((a, b) => append(a, b))

}