package com.benchmark.fun

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.stats.distributions._

object Benchmark {

  // Sphere
  def f1(x: Array[Double]) = {
    sum(x.map(pow(_, 2)))
  }

  // Schwefel 2.22
  def f2(x: Array[Double]) = {

    sum(x.map { _.abs }) + x.map { _.abs }.product
  }

  // Schwefel 1.2
  def f3(x: Array[Double]) = {

    val dim = x.length

    var result = 0.0
    for (i <- 1 to dim) {
      result = result + pow(sum(x.slice(0, i)), 2)
    }

    result
  }

  // Schwefel 2.21
  def f4(x: Array[Double]) = {

    max(x.map { _.abs })
  }

  // Rosenbrock
  def f5(x: Array[Double]) = {

    val dim = x.length

    val tmp = x.slice(0, dim - 1)

    sum(pow((x.slice(1, dim) - pow(tmp, 2)), 2).map(_ * 100).zip(pow(tmp.map(_ - 1), 2)).map { case (a, b) => a + b })
  }

  // Rastrigin
  def f6(x: Array[Double]) = {

    val dim = x.length

    sum(x.map(i=>pow(i, 2)-10*cos(2*i*(constants.Pi))+10))
  }

  // Ackley
  def f7(x: Array[Double]) = {

    val dim = x.length

    -20*exp(-0.2*sqrt(sum(x.map(pow(_, 2)))/dim)) - exp(sum(x.map(i=>cos(2*i*(constants.Pi))))/dim)+20+exp(1.0)
  }

  // Griewangk
  def f11(x: Array[Double]) = {

    (sum(x.map(agent => pow(agent, 2)))/4000) - x.zipWithIndex.map { case (agent, idx) => cos(agent/sqrt(idx+1.0)) }.product + 1.0
  }

  // F16 (CF3)
  def f16(x: Array[Double]) = {

    4*(pow(x(0), 2))-2.1*(pow(x(0), 4))+(pow(x(0), 6))/3+x(0)*x(1)-4*(pow(x(1), 2))+4*(pow(x(1), 4))
  }

  // F17 (CF4)
  def f17(x: Array[Double]) = {

    pow((x(1)-(pow(x(0), 2))*5.1/(4*(pow(constants.Pi, 2)))+5/constants.Pi*x(0)-6), 2)+10*(1-1/(8*constants.Pi))*cos(x(0))+10;
  }

  // F18 (CF5)
  def f18(x: Array[Double]) = {

    (1+pow((x(0)+x(1)+1), 2)*(19-14*x(0)+3*(pow(x(0), 2))-14*x(1)+6*x(0)*x(1)+3*pow(x(1), 2)))*(30+pow((2*x(0)-3*x(1)), 2)*(18-32*x(0)+12*(pow(x(0), 2))+48*x(1)-36*x(0)*x(1)+27*(pow(x(1), 2))));
  }

  def Get_Functions_details(fun: String, x: Array[Double]): (Double, Double, Int, Double) = {

    fun match {
      case "f1"  => return (-100.0, 100.0, 30, f1(x))
      case "f2"  => return (-10.0, 10.0, 30, f2(x))
      case "f3"  => return (-100.0, 100.0, 30, f3(x))
      case "f4"  => return (-100.0, 100.0, 30, f4(x))
      case "f5"  => return (-30.0, 30.0, 30, f5(x))
      case "f6"  => return (-5.12, 5.12, 30, f6(x))
      case "f7"  => return (-32, 32, 30, f7(x))
      case "f11" => return (-600.0, 600.0, 30, f11(x))
      case "f16" => return (-5.0, 5.0, 30, f16(x))
      case "f17" => return (-5.0, 5.0, 30, f17(x))
      case "f18" => return (-5.0, 5.0, 30, f18(x))
      case _    => return (0.0, 0.0, 0, 0.0)
    }
  }
}
