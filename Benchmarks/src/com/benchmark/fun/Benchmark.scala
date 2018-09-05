package com.benchmark.fun

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.stats.distributions._

object Benchmark {

  def f1(x: Array[Double]) = {
    sum(x.map(pow(_, 2)))
  }

  def f2(x: Array[Double]) = {

    sum(x.map { _.abs }) + x.map { _.abs }.product
  }

  def f3(x: Array[Double]) = {

    val dim = x.length

    var result = 0.0
    for (i <- 1 to dim) {
      result = result + pow(sum(x.slice(0, i)), 2)
    }

    result
  }

  def f4(x: Array[Double]) = {

    max(x.map { _.abs })
  }

  def f5(x: Array[Double]) = {

    val dim = x.length

    val tmp = x.slice(0, dim - 1)

    sum(pow((x.slice(1, dim) - pow(tmp, 2)), 2).map(_ * 100).zip(pow(tmp.map(_ - 1), 2)).map { case (a, b) => a + b })
  }

  def Get_Functions_details(fun: String, x: Array[Double]): (Double, Double, Int, Double) = {

    fun match {
      case "f1" => return (-100.0, 100.0, 30, f1(x))
      case "f2" => return (-10.0, 10.0, 30, f2(x))
      case "f3" => return (-100.0, 100.0, 30, f3(x))
      case "f4" => return (-100.0, 100.0, 30, f4(x))
      case "f5" => return (-30.0, 30.0, 30, f5(x))
      case _    => return (0.0, 0.0, 0, 0.0)
    }
  }
}