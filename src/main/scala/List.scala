package com.tkroman.kpi.y2022.l1

import scala.annotation.tailrec

enum Compared:
  case Lt, Gt, Eq // < > =

enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

  def getHead: A = {
    this match {
      case Nil => 
        throw new RuntimeException("NO HEAD!")
      case Cons(head, tail) => head
    }
  }

  def getTail: List[A] = {
    this match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }
  }

  def length[B >: A]: Int = {
    @tailrec
    def go(list: List[A], acc: Int): Int = {
      list match {
        case Nil => acc
        case Cons(head, tail) => go(tail, acc + 1)
      }
    }
    go(this, 0)
  }

  def reverse: List[A] = {
    @tailrec
    def go(list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case Cons(head, tail) => 
          go(tail, Cons(head, acc))
      }
    }
    go(this, Nil)
  }

  def concatenate[B >: A](list: List[B]): List[B] = {
    @tailrec
    def go(base: List[B], acc: List[B]): List[B] = {
      base match {
        case Nil => acc
        case Cons(head, tail) =>
          go(tail, Cons(head, acc.reverse).reverse)
      }
    }
    go(list, this)
  }

  def partition(compare: (A, A) => Compared): (List[A], List[A]) = {
    @tailrec
    def go(
        list: List[A], 
        p: A, 
        left: List[A],
        right: List[A],
        comp: (A, A) => Compared
    ): (List[A], List[A]) = {
      list match {
        case Nil => (Cons(p, left.reverse).reverse, right)
        case Cons(head, tail) =>
          if comp(p, head) == Compared.Lt
          then
            go(tail, p, left, Cons(head, right.reverse).reverse, comp)
          else
            go(tail, p, Cons(head, left.reverse).reverse, right, comp)
      }
    }
    go(this.getTail, this.getHead, Nil, Nil, compare)
  }

  def quickSort(compare: (A, A) => Compared): List[A] ={
    def go(comp: (A, A) => Compared, list: List[A]): List[A] ={
      if list.length == 0 || list.length == 1 then list
      else 
        val (left, right) = list.partition(comp)
        go(comp, left).concatenate(go(comp, right))        
    }
    go(compare, this)
  }

  def flatMap[B](f: A => List[B]): List[B] = {
    @tailrec
    def go(list: List[A], acc: List[B]): List[B] = {
      list match {
        case Nil => acc
        case Cons(head, tail) => 
          go(tail, acc.concatenate(f(head)))
      }
    }
    go(this, Nil)
  }

  def map[B](f: A => B): List[B] = {
    @tailrec
    def go(list: List[A], acc: List[B]): List[B] = {
      list match {
        case Nil => acc
        case Cons(head, tail) => 
          go(tail, Cons(f(head), acc.reverse).reverse)
      }
    }
    go(this, Nil)
  }

  def tails: List[List[A]] = {
    @tailrec
    def go(list: List[A], acc: List[List[A]]): List[List[A]] = {
      list match{
        case Nil => acc
        case Cons(head, tail) => go(tail, Cons(list, acc.reverse).reverse)
      }
    }
    go(this, Nil)
  }

  override def toString: String =
    @scala.annotation.tailrec 
    def go(sb: StringBuilder, as: List[A]): String = {
      as match {
        case Nil => 
          sb.result
        case Cons(hd, tl) => 
          go(
            sb
              .append(hd)
              .append(if tl == Nil then "]" else ", "), 
            tl
          )
      }
    }
    this match{
      case Nil => 
        var sb: StringBuilder = new StringBuilder("[]")
        sb.result
      case Cons(hd, tl) =>
        go(new StringBuilder("["), this)
    }

import List.*
object List:
  
  def apply[A](xs: A*) = of(xs*)

  def of[A](xs: A*): List[A] = {
    xs.foldRight(Nil: List[A]) { case (x, res) => Cons(x, res) }
  }

  def product[A](lists: List[List[A]]): List[List[A]] = {
    @tailrec 
    def go(lists: List[List[A]], acc: List[List[A]]): List[List[A]] = {
      lists match {
        case Nil => acc
        case Cons(head, tail) => 
          go(tail, head.flatMap(h => acc.map(a => Cons(h, a))))
      }
    }
    go(lists.reverse, List(List()))
  }

@main def run() =
  println(
    "Running smoothly"
  )
  //var List1: List[Int] = Nil//List(4, 2, 6, 1, 5, 3)
  //println(List1.quickSort())
  // var LOL2: List[List[Int]] = List(List(1, 2), List(3, 4), List(5,6))
  // println(List1.quickSort((a: Int, b: Int) =>
  //   if a > b then Compared.Gt
  //   else if a < b then Compared.Lt
  //   else Compared.Eq
  // ))
  // println(product(LOL2))
  // println(List1.tails)