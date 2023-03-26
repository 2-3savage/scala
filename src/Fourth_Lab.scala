import scala.io.StdIn.{readDouble as rd, readInt as ri, readLine as rl}
import scala.collection.mutable.ListBuffer
import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.util.control.*

object Fourth_Lab {
  sealed trait Estate

  case class Flat(area: Int, floor: Int, floorsHouse: Int, price: Int) extends Estate {
    def display(): Unit = {
      println("Площадь квартиры: " + area)
      println("Номер этажа квартиры: " + floor)
      println("Этажность дома: " + floorsHouse)
      println("Стоимость квартиры: " + price)
      println()
    }
  }
  case class Room(area_room: Int, area: Int, floor: Int, floorsHouse: Int, price: Int, priceRoom: Int) extends Estate {
    def display(): Unit = {
      println("Комната в квартире:")
      println("Площадь комнаты: " + area_room)
      println("Стоимость комнаты: " + priceRoom)
      println("Квартира:")
      println("Площадь квартиры: " + area)
      println("Номер этажа квартиры: " + floor)
      println("Этажность дома: " + floorsHouse)
      println("Стоимость квартиры: " + price)
      println()
    }
  }
  case class Home(area: Int, price: Int) extends Estate {
    def display(): Unit = {
      println("Площадь дома: " + area)
      println("Стоимость дома: " + price)
      println()
    }
  }

  def getByPrice(list: ListBuffer[Estate], fixPrice: Int): Unit = {
    for (element <- list)
      element match
        case home: Home =>
          if home.price < fixPrice then
            println(home.display())
        case room: Room =>
          if room.priceRoom < fixPrice then
            println(room.display())
        case flat: Flat =>
          if flat.price < fixPrice then
            println(flat.display())
  }

  def getByLevel(list: ListBuffer[Estate], level: Int): Unit = {
    list.filter(_.isInstanceOf[Flat]).filter(_.asInstanceOf[Flat].floor == level)
      .foreach(_.asInstanceOf[Flat].display())
  }

  def getExceptBounds(list: ListBuffer[Estate]): Unit = {
    list.filter(_.isInstanceOf[Flat]).filter(x => x.asInstanceOf[Flat].floor != x.asInstanceOf[Flat].floorsHouse)
      .filter(_.asInstanceOf[Flat].floor != 1).foreach(_.asInstanceOf[Flat].display())
  }

  def query(query: Query, list: ListBuffer[Estate]): Unit = {
    for (element <- list)
      element match
        case home: Home =>
          if query.typeObject == 3 then
            if (home.price < query.max_price) && (home.area > query.min_area) then
              println(home.display())
        case room: Room =>
          if query.typeObject == 2 then
            if (room.priceRoom < query.max_price) && (room.area_room > query.min_area) &&
              (query.max_floor > room.floor) && (room.floor > query.min_floor) then
              println(room.display())
        case flat: Flat =>
          if query.typeObject == 1 then
            if (flat.price < query.max_price) && (flat.area > query.min_area) && (flat.floor > query.min_floor)
              && (flat.floor < query.max_floor) then
              println(flat.display())
  }
  def getHouses(list: ListBuffer[Estate]): Unit = {
    list.filter(_.isInstanceOf[Home]).foreach(_.asInstanceOf[Home].display())
  }
  case class Query(typeObject: Int, min_area: Int = 0, max_price: Int = 0, min_floor: Int = -1, max_floor: Int = -1)

  @main
  def main(): Unit = {
    val file = File("D:\\scala\\file.csv")
    var list = new ListBuffer[Estate]

    list += Flat(5, 25, 10, 25000)
    list += Flat(2, 23, 5, 250000)
    list += Flat(3, 21, 5, 85000)
    list += Flat(4, 54, 5, 65000)
    list += Flat(1, 12, 5, 55000)
    list += Flat(6, 65, 8, 45000)
    list += Flat(7, 26, 9, 35000)
    list += Flat(8, 43, 8, 15000)
    list += Flat(9, 34, 10, 25000)
    list += Flat(10, 8, 10, 54000)

    list += Home(120, 12000)
    list += Home(1200, 120000)
    list += Home(566, 1123000)
    list += Home(32, 4232000)
    list += Home(645, 22000)
    list += Home(123, 52000)
    list += Home(1563, 672000)
    list += Home(1230, 92000)

    list += Room(15, 5, 25, 10, 25000, 1233)
    list += Room(3, 2, 23, 5, 250000, 12)
    list += Room(4, 3, 21, 5, 85000, 1432)
    list += Room(32, 4, 54, 5, 65000, 1234)
    list += Room(2, 1, 12, 5, 55000, 7465)
    list += Room(22, 6, 65, 8, 45000, 657)
    list += Room(12, 7, 26, 9, 35000, 3125)
    list += Room(32, 8, 43, 8, 15000, 345)
    list += Room(23, 9, 34, 10, 25000, 3456)
    list += Room(1, 10, 8, 10, 54000, 123)


    val loop = new Breaks
    val nestedLoop = new Breaks
    loop.breakable {
      while true do
        println("Выберите пункт:")
        println("0. Завершить программу")
        println("1. Добавить новый элемент")
        println("2. Вывести из базы данных все частные дома")
        println("3. Вывести только объекты недвижимости (вы должны будете указать цену)")
        println("4. Вывести из базы данных квартиры на указанном этаже")
        println("5. Вывести из базы квартиры, не находящиеся на первом и последнем этаже")
        println("6. Выбрать из базы данных по списку требований")
        println("7. Сохранить базу в файл")
        println("8. Вывести из файла базу")
        val point = ri()
        point match
          case 0 =>
            loop.break()
          case 1 =>
            println("Выберите, что хотите ввести:")
            println("1. Квартира")
            println("2. Дом")
            val point2 = ri()
            point2 match
              case 1 =>
                println("Введите этажность дома:")
                val floorsHouse = ri()
                println("Введите этаж дома:")
                val floor = ri()
                println("Введите площадь квартиры:")
                val area = ri()
                println("Введите стоимость квартиры:")
                val price_floor = ri()
                val flat = Flat(floor, area, floorsHouse, price = price_floor)
                list += flat
                nestedLoop.breakable {
                  while true do
                    println("Хотите добавить комнаты в квартиру?")
                    println("1. Да")
                    println("2. Нет")
                    val point3 = ri()
                    point3 match
                      case 1 =>
                        println("Введите квадраты комнаты")
                        val areaRoom = ri()
                        println("Введите стоимость комнаты:")
                        val price_room = ri()
                        val room = Room(areaRoom, floor, area, floorsHouse, price_floor, price_room)
                        list += room
                      case 2 =>
                        println("Вы успешно добавили квартиру в базу данных!")
                        nestedLoop.break
                      case _ =>
                        println("Вы неправильно ввели данные. Вы успешно добавили квартиру в базу данных!")
                        nestedLoop.break
                }

              case 2 =>
                println("Введите площадь дома:")
                val area = ri()
                println("Введите цену дома:")
                val price = ri()
                val home = Home(area, price)
                list += home
                println("Вы успешно добавили дом в базу данных!")
              case _ =>
                println("Вы неправильно выбрали данные! Попробуйте еще раз!")
          case 2 => getHouses(list)
          case 3 =>
            println("Введите максимальную цену")
            val fixPrice = ri()
            getByPrice(list, fixPrice)
          case 4 =>
            println("Введите этаж")
            val level = ri()
            getByLevel(list, level)
          case 5 =>
            getExceptBounds(list)
          case 6 =>
            println("Выберите желаемый тип объекта:")
            println("1. Квартира")
            println("2. Комната ")
            println("3. Дом")
            val point3 = ri()
            point3 match
              case 1 =>
                println("Введите желаемую минимальную площадь")
                val min_area = ri()
                println("Введите максимальную цену")
                val max_price = ri()
                println("Введите минимальное ограничение на этаж")
                val min_floor = ri()
                println("Введите максимально ограничение на этаж")
                val max_floor = ri()
                query(Query(1, min_area, max_price, min_floor, max_floor), list)
              case 2 =>
                println("Введите желаемую минимальную площадь")
                val min_area = ri()
                println("Введите максимальную цену")
                val max_price = ri()
                println("Введите минимальное ограничение на этаж")
                val min_floor = ri()
                println("Введите максимально ограничение на этаж")
                val max_floor = ri()
                query(Query(2, min_area, max_price, min_floor, max_floor), list)
              case 3 =>
                println("Введите желаемую минимальную площадь")
                val min_area = ri()
                println("Введите максимальную цену")
                val max_price = ri()
                query(Query(3, min_area, max_price), list)
          case 7 =>
            write(file, list)
          case 8 =>
            list = read(file)
          case _ =>
            println("Вы неправильно ввели данные! Попробуйте еще раз!")
    }
  }


  trait Serializable[T]:
    def serialize(item: T): String

  object Serializable:
    implicit object SerializableFlat extends Serializable[Flat] {
      override def serialize(item: Flat): String =
        s"\"FLAT\", \"${item.area}\", \"${item.floor}\", \"${item.floorsHouse}\", \"${item.price}\"\n"
    }

    implicit object SerializableRoom extends Serializable[Room] {
      override def serialize(item: Room): String =
        s"\"ROOM\", \"${item.area_room}\", \"${item.price}\", \"${item.floor}\", \"${item.area}\", \"${item.floorsHouse}\", \"${item.priceRoom}\"\n"

    }

    implicit object SerializableHome extends Serializable[Home] {
      override def serialize(item: Home): String =
        s"\"HOME\", \"${item.area}\", \"${item.price}\"\n"

    }

    implicit object SerializableEstate extends Serializable[Estate] :
      override def serialize(item: Estate): String =
        item match
          case flat: Flat => SerializableFlat.serialize(flat)
          case room: Room => SerializableRoom.serialize(room)
          case home: Home => SerializableHome.serialize(home)


  def deserialize(line: String): Option[Estate] =
    val row = line.split(",").map(_.trim).toList
    row.head match
      case "\"HOME\"" =>
        val area = row(1).split("\"").toList(1).toInt
        val price = row(2).split("\"").toList(1).toInt
        Some(Home(area, price))
      case "\"ROOM\"" =>
        val area_room = row(1).split("\"").toList(1).toInt
        val price = row(2).split("\"").toList(1).toInt
        val floor = row(3).split("\"").toList(1).toInt
        val area = row(4).split("\"").toList(1).toInt
        val floorsHouse = row(5).split("\"").toList(1).toInt
        val priceRoom = row(6).split("\"").toList(1).toInt
        Some(Room(area_room, area, floor, floorsHouse, price, priceRoom))
      case "\"FLAT\"" =>
        val area = row(1).split("\"").toList(1).toInt
        val floor = row(2).split("\"").toList(1).toInt
        val floorsHouse = row(3).split("\"").toList(1).toInt
        val price = row(4).split("\"").toList(1).toInt
        Some(Flat(area, floor, floorsHouse, price))
      case _ =>
        None

  def read(file: File): ListBuffer[Estate] =
    val list = new ListBuffer[Estate]
    val source = Source.fromFile(file)
    val items = source.getLines.flatMap(deserialize).toList
    for (element <- items){
      list += element
    }
    source.close
    list

  def write[T](file: File, items: ListBuffer[T])(implicit serializable: Serializable[T]): Unit = {
    val writer = new BufferedWriter(new FileWriter(file))
    items.map(serializable.serialize).foreach(writer.write)
    writer.close()
  }

}