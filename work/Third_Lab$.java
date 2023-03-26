object Third_Lab {
  @main
  def main(args: String): Unit = {
    val list = new ListBuffer[Any]

    list += new Flat(5, 25, 10, 25000)
    list += new Flat(2, 23, 5, 250000)
    list += new Flat(3, 21, 5, 85000)
    list += new Flat(4, 54, 5, 65000)
    list += new Flat(1, 12, 5, 55000)
    list += new Flat(6, 65, 8, 45000)
    list += new Flat(7, 26, 9, 35000)
    list += new Flat(8, 43, 8, 15000)
    list += new Flat(9, 34, 10, 25000)
    list += new Flat(10, 8, 10, 54000)

    list += new Home(120, 12000)
    list += new Home(1200, 120000)
    list += new Home(566, 1123000)
    list += new Home(32, 4232000)
    list += new Home(645, 22000)
    list += new Home(123, 52000)
    list += new Home(1563, 672000)
    list += new Home(1230, 92000)

    list += new Room(15, Flat(5, 25, 10, 25000), 1233)
    list += new Room(3, Flat(2, 23, 5, 250000), 12)
    list += new Room(4, Flat(3, 21, 5, 85000), 1432)
    list += new Room(32, Flat(4, 54, 5, 65000), 1234)
    list += new Room(2, Flat(1, 12, 5, 55000), 7465)
    list += new Room(22, Flat(6, 65, 8, 45000), 657)
    list += new Room(12, Flat(7, 26, 9, 35000), 3125)
    list += new Room(32, Flat(8, 43, 8, 15000), 345)
    list += new Room(23, Flat(9, 34, 10, 25000), 3456)
    list += new Room(1, Flat(10, 8, 10, 54000), 123)

    val loop = new Breaks
    val nestedLoop = new Breaks
    loop.breakable {
      while true do
        val agency = new RealEstateAgency()
        println("Выберите пункт:")
        println("0. Завершить программу")
        println("1. Добавить новый элемент")
        println("2. Вывести из базы данных все частные дома")
        println("3. Вывести только объекты недвижимости (вы должны будете указать цену)")
        println("4. Вывести из базы данных квартиры на указанном этаже")
        println("5. Вывести из базы квартиры, не находящиеся на первом и последнем этаже")
        println("6. Выбрать из базы данных по списку требований:")
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
                val flat = new Flat(floor, area, floorsHouse, price = price_floor)
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
                        val room = Room(areaRoom, flat, price_room)
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
                val home = new Home(area, price)
                list += home
                println("Вы успешно добавили дом в базу данных!")
              case _ =>
                println("Вы неправильно выбрали данные! Попробуйте еще раз!")

          case 2 =>
            agency.getHouses(list)
          case 3 =>
            println("Введите максимальную цену")
            val fixPrice = ri()
            agency.getByPrice(list, fixPrice)
          case 4 =>
            println("Введите этаж")
            val level = ri()
            agency.getByLevel(list, level)
          case 5 =>
            agency.getExceptBounds(list)
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
                agency.query(new Query(1, min_area, max_price, min_floor, max_floor), list)
              case 2 =>
                println("Введите желаемую минимальную площадь")
                val min_area = ri()
                println("Введите максимальную цену")
                val max_price = ri()
                println("Введите минимальное ограничение на этаж")
                val min_floor = ri()
                println("Введите максимально ограничение на этаж")
                val max_floor = ri()
                agency.query(new Query(2, min_area, max_price, min_floor, max_floor), list)
              case 3 =>
                println("Введите желаемую минимальную площадь")
                val min_area = ri()
                println("Введите максимальную цену")
                val max_price = ri()
                agency.query(new Query(3, min_area, max_price), list)
          case _ =>
            println("Вы неправильно ввели данные! Попробуйте еще раз!")


    }
  }

  class RealEstateAgency {
    def getHouses(list: ListBuffer[Any]): Unit = {
      list.filter(_.isInstanceOf[Home]).foreach(_.asInstanceOf[Home].display())
    }

    def getByPrice(list: ListBuffer[Any], fixPrice: Int): Unit = {
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

    def getByLevel(list: ListBuffer[Any], level: Int): Unit = {
      list.filter(_.isInstanceOf[Flat]).filter(_.asInstanceOf[Flat].floor == level)
        .foreach(_.asInstanceOf[Flat].display())
    }

    def getExceptBounds(list: ListBuffer[Any]): Unit = {
      list.filter(_.isInstanceOf[Flat]).filter(x => x.asInstanceOf[Flat].floor != x.asInstanceOf[Flat].floorsHouse)
        .filter(_.asInstanceOf[Flat].floor != 1).foreach(_.asInstanceOf[Flat].display())
    }

    def query(query: Query, list: ListBuffer[Any]): Unit = {
      for (element <- list)
        element match
          case home: Home =>
            if query.typeObject == 3 then
              if (home.price < query.max_price) && (home.area > query.min_area) then
                println(home.display())
          case room: Room =>
            if query.typeObject == 2 then
              if (room.priceRoom < query.max_price) && (room.area_room > query.min_area) &&
                (query.max_floor > room.asInstanceOf[Flat].floor) && (room.asInstanceOf[Flat].floor > query.min_floor) then
                println(room.display())
          case flat: Flat =>
            if query.typeObject == 1 then
              if (flat.price < query.max_price) && (flat.area > query.min_area) && (flat.floor > query.min_floor)
                && (flat.floor < query.max_floor) then
                println(flat.display())
    }


  }

  class Flat(val floor: Int, val area: Int, val floorsHouse: Int, val price: Int){
    def display(): Unit = {
      println("Площадь квартиры: " + area)
      println("Номер этажа квартиры: " + floor)
      println("Этажность дома: " + floorsHouse)
      println("Стоимость квартиры: " + price)
      println()
    }
  }
  class Room(val area_room: Int, flat: Flat, val priceRoom: Int){
    def display(): Unit = {
      println("Комната в квартире:")
      println("Площадь комнаты: " + area_room)
      println("Стоимость комнаты: " + priceRoom)
      println("Квартира:")
      println("Площадь квартиры: " + flat.area)
      println("Номер этажа квартиры: " + flat.floor)
      println("Этажность дома: " + flat.floorsHouse)
      println("Стоимость квартиры: " + flat.price)
      println()
    }
  }

  class Home(val area: Int, val price: Int){
    def display(): Unit ={
      println("Площадь дома: " + area)
      println("Стоимость дома: " + price)
      println()
    }
  }
  class Query(val typeObject: Int, val min_area: Int = 0, val max_price: Int = 0, val min_floor: Int = -1, val max_floor: Int = -1)

}
