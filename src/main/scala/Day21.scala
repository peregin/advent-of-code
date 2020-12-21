

/**
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
 */
object Day21 extends Aso("input21.txt", identity) {


  val pattern = """(.*) \(contains (.*)\)""".r

  case class Food(ingredients: Set[String], allergens: Set[String]) {
    lazy val allergen2Ingredient = allergens.map(a => a -> ingredients).groupMapReduce(_._1)(_._2)(_ intersect _)
  }

  val entries = input.map{
    case pattern(ingredients, allergens) => Food(ingredients.split(' ').toSet, allergens.split(' ').toSet)
  }

  val solution1 = ???
  println(s"solution1=$solution1")
}
