import com.smart.pizza.RestaurantManager._
import org.scalatest._

/**
 * Created by Aleksey on 20.07.2015.
 */
class RestaurantTest extends FlatSpec with Matchers {


  "Restaurant" should "process test input correctly" in {
    {
      val queue: CustomersQueue = Customer(0, 3) #:: Customer(1, 9) #:: Customer(2, 6) #:: Stream.Empty
      Restaurant(queue).processOrders should be(9)
    }

    {
      val queue: CustomersQueue = Customer(0, 3) #:: Customer(1, 9) #:: Customer(2, 5) #:: Stream.Empty
      Restaurant(queue).processOrders should be(8)
    }
  }


}
