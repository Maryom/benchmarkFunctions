package com.benchmark.fun

object TryMe {

  /** Our main function where the action happens */
  def main(args: Array[String]) {

    val testArray = Array(16.39819879, 9.82832027, -23.95491008, 9.44969793)

    println(Benchmark.Get_Functions_details("f5", testArray))
  }
}