package nl.hugo.redbook.ch9

object Exercise10 {

  // Given: "abra" ** " ".many ** "cadabra"
  // What error to give on "cAdabra"
  // => Error on "cAdabra", expected "cadabra"

  // Given: a | b
  // If a fails, how to choose whether to execute b or not?
  // => a.recover | b

  // How to report location of errors?
  // => "abra cAdabra"
  //          ^
  // => "abra cAdabra"
  //    Error at line 1:6

  // Given: a | b
  // If a and b fail, how to choose which error(s) to report?
  // => (a | b).reportFirst
  //    (a | b).reportLast
  //    (a | b).reportAll
}