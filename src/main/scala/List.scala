package com.tkroman.kpi.y2022.l1
import scala.annotation.tailrec
import scala.collection.mutable
import scala.runtime.Nothing$

enum List[+A] {
  case Nil
  case Cons(hd: A, tl: List[A])

  override def toString: String = {
    @tailrec
    def go(sb: mutable.StringBuilder, as: List[A]): String = {
      as match {
        case Nil => sb.result
        case Cons(h, t) => go(sb.append(h).append(if t == Nil then "]" else ", "), t)
      }
    }
    go(new mutable.StringBuilder("["), this)
  }

  def reverse: List[A] = {
    @tailrec
    def go(xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(xh, xt) => go(xt, Cons(xh, acc))
    }
    go(this, Nil)
  }

  def concatenate[B >: A] (xs: List[B]): List[B] = {
    var Res: List[B] = this
    @tailrec
    def go(xo: List[B]): List[B] ={
      xo match {
        case Nil => Res
        case Cons(xh,xt) =>
          Res = Cons(xh, Res.reverse).reverse
          go(xt)
      }
    }
    go(xs)
  }

  def foldLeft[B](x: B)(f: (B, A) => B): B = {
    @tailrec
    def go(xs: List[A], acc: B): B = xs match {
      case Nil => acc
      case Cons(xh, xt) => go(xt, f(acc, xh))
    }
    go(this, x)
  }

  def zipWithIndex: List[(A, Int)] = {
    @tailrec
    def go(xs: List[A], ys: Int, acc: List[(A, Int)]): List[(A, Int)] = (xs, ys) match {
      case (Nil, _) => acc.reverse
      case (Cons(xh, xt: List[A]), yh: Int) =>
        val pair = (xh, yh)
        go(xt, yh + 1, Cons(pair, acc))
    }
    go(this, 0, Nil)
  }

  def updated[S >: A](a: S, n: Int): List[S] = {
    @tailrec
    def go[S >: A](xs: List[A], a: S, i: Int, acc: List[S]): List[S] = xs match {
      case Nil => acc.reverse
      case Cons(hd, tl) =>
        if i == n then
          go(tl, a, i + 1, Cons(a, acc))
        else
        {
          go(tl, a, i + 1, Cons(hd, acc))
        }
    }
    go(this, a, 0, Nil)
  }

  def collect[B](f: A => Option[B]): List[B] = {
    @tailrec
    def go(xs: List[A], acc: List[B]): List[B] = xs match{
      case Nil => acc.reverse
      case Cons(hd, tl) => f(hd) match{
        case None => go(tl, acc)
        case Some(x) => go(tl, Cons(x, acc))
      }
    }
    go(this, Nil)
  }

  def flatMap[B](f: A => List[B]): List[B] = {
    @tailrec
    def go(xs: List[A], acc: List[B]): List[B] = xs match {
      case Nil => acc
      case Cons(hd, tl) => go(tl, acc.concatenate(f(hd)))
    }
    go(this, Nil)
  }

  def unpack[A](xs: List[List[A]]): List[A] = {
    @tailrec
    def go(xs: List[List[A]], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(hd, tl) => go(tl, acc.concatenate(hd))
    }
    go(xs, Nil)
  }

  def listLength: Int = {
    @tailrec
    def go(xs: List[A], acc: Int): Int = xs match {
      case Nil => acc
      case Cons(hd, tl) => go(tl, acc+1)
    }
    go(this, 0)
  }

  def elemOnPos(pos: Int): A = {
    @tailrec
    def go(xs: List[A], i: Int): A = xs match {
      case Nil => throw new RuntimeException("NO ELEM")
      case Cons(hd, tl) =>
        if i == pos then hd
        else
          go(tl, i+1)
    }
    go(this, 0)
  }

  def transpose[A](xs: List[List[A]]): List[List[A]] = {
    val unpacked: List[A] = xs.unpack(xs)
    val xsLength = xs.listLength
    val step: Int = xs match{
      case Nil => 0
      case Cons(hd, tl) => hd.listLength
    }
    @tailrec
    def go(xs: List[A], i: Int, acc: List[List[A]]): List[List[A]] = {
        @tailrec
        def rec(ys: List[A], k: Int, res: List[A]): List[A] = {
          if k == xsLength then
            res.reverse
          else
            rec(unpacked, k + 1, Cons(ys.elemOnPos(i + k*step), res))
        }
        if i == step - 1 then
          Cons(rec(unpacked, 0, Nil), acc).reverse
        else
          go(unpacked, i+1, Cons(rec(unpacked, 0, Nil), acc))
    }
    go(unpacked, 0, Nil)
  }
}



import List.*
object List:
  def apply[A](xs: A*): List[A] = of(xs*)
  def of[A](xs: A*): List[A] =
    xs.foldRight(Nil: List[A]) { case (x, acc) => Cons(x, acc) }


@main def run(): Unit =
  println("Hello")




