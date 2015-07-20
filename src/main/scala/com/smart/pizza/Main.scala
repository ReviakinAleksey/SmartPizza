package com.smart.pizza

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

/**
 * Created by Aleksey on 17.07.2015.
 */

object RestaurantManager {

  type CustomersQueue = Stream[Customer]

  type QueueCombinationWithTime = (Int, Long)



  /**
   * Restaurant Customer
   * @param arrivalTime time when customer arrived to restaurant
   * @param orderTime time time required to cook pizza
   */
  case class Customer(arrivalTime: Long, orderTime: Long)


  /**
   * Restaurant Cook
   * @param time current time
   * @param overallWaitingTime sum of waiting time currently processed orders 
   * @param readyOrders number of currently processed orders
   */
  case class Cook(time: Long = 0, overallWaitingTime: Long = 0, readyOrders: Long = 0) {

    /**
     * Calculate current average waiting time
     * @return average waiting time
     */
    def calculateAverageTime: Long = overallWaitingTime / readyOrders

    /**
     * Customer order processing
     * @param customer customer whose order will be processed by cook
     * @return Updated cook state on time customer order ready
     */
    def processOrder(customer: Customer): Cook = {
      //     Math.max(time, customer.arrivalTime) we must take into account situation when customer arrived after some time upon previous order, hence cook has a little tim to rest
      val nextTime: Long = Math.max(time, customer.arrivalTime) + customer.orderTime

      copy(time = nextTime,
        overallWaitingTime = this.overallWaitingTime + (nextTime - customer.arrivalTime),
        readyOrders = this.readyOrders + 1)
    }

  }


  /**
   * Restaurant combines Customers with Cook
   * @param inputQueue current customers input queue
   * @param waitingCustomers customers that already waits for order completion
   * @param cook Cook which makes pizza
   */
  case class Restaurant(inputQueue: CustomersQueue, waitingCustomers: List[Customer] = Nil, cook: Cook = Cook()) {


    private val ord: Ordering[QueueCombinationWithTime] = new Ordering[QueueCombinationWithTime] {
      override def compare(x: QueueCombinationWithTime, y: QueueCombinationWithTime): Int = Ordering.Long.compare(x._2, y._2)
    }

    /**
     * Manages restaurant state
     * @return Restaurant instance with optimally organized waiting customers,
     *         and new inputQueue without customers that already arrived at restaurant
     */
    private def processInputQueue: Restaurant = {

      // Find if any new customers arrived at restaurant
      var (arrivedCustomers: CustomersQueue, newInputQueue: CustomersQueue) = inputQueue.span(_.arrivalTime <= cook.time)

      // If no one is arrived at this time and there is no waiting customers in restaurant, take one customer from future
      if (arrivedCustomers.isEmpty && waitingCustomers.isEmpty && newInputQueue.nonEmpty) {
        arrivedCustomers = Stream(newInputQueue.head)
        newInputQueue = newInputQueue.tail
      }

      //Concat arrived customers anc customers that are already wait for orders
      val newCustomers: List[Customer] = waitingCustomers ++ arrivedCustomers

      //minimalCombination - combination of customers order with minimal overall cooking time
      val (minimalCombinationIndex:Int, _) = newCustomers
        .permutations //For all possible variants of customer orders processing
        .zipWithIndex //Index used to reduce memory usage (not store all possible customer orders processing variant)
        .map({
          case (variant, index) =>
            //Number that maps to current customers order processing time
            val variantOrderTime: Long = variant.zipWithIndex.foldLeft(0L) {
              case (acc, (customer, elementIndex)) =>
                acc + ((newCustomers.length - elementIndex) * customer.orderTime)
            }
            index -> variantOrderTime
      }).min(ord) //Find minimal customers order processing variant index

      //Build minimal customers order processing variant from index
      val minimalCombination = newCustomers.permutations.drop(minimalCombinationIndex).next()

      copy(inputQueue = newInputQueue,
        waitingCustomers = minimalCombination)
    }

    @tailrec
    final def processOrders: Long = {
      //Check if new customers is arrived, find optimal processing combination for current customers queue
      val restaurant = processInputQueue

      restaurant.waitingCustomers match {
        case customer :: waitingQueue =>
          //Process order by Cook and step next in to customers queue
          restaurant.copy(cook = cook.processOrder(customer),
            waitingCustomers = waitingQueue).processOrders
        case Nil =>
          //There is no waiting customers and input queue is empty
          //Calculate result
          cook.calculateAverageTime

      }
    }
  }

}

object QueueParser {
  def parse(fileName: String): RestaurantManager.CustomersQueue = {
    val file: BufferedSource = Source.fromFile(fileName)
    try {
      val lines: Iterator[String] = file.getLines()
      val customersCount: Int = lines.next().toInt
      (0 until customersCount).map((x) => {
        val line: String = lines.next()
        val Array(arrivalTime, orderTime) = line.split("\\s").map(_.toInt)
        RestaurantManager.Customer(arrivalTime, orderTime)
      }).toStream
    } finally {
      file.close()
    }

  }
}


object Main extends App {
  private val targetFileName: String = args(0)
  println(s"Calculating average waiting time for file: ${targetFileName}")

  import RestaurantManager._

  private val customersQueue: CustomersQueue = QueueParser.parse(targetFileName)
  println(Restaurant(customersQueue).processOrders)
}
