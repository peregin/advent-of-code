package aso.aso2020

import aso.Aso

/**
  * mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
  * trh fvjkl sbzzf mxmxvkd (contains dairy)
  * sqjhc fvjkl (contains soy)
  * sqjhc mxmxvkd sbzzf (contains fish)
  */
object Day21 extends Aso("aso2020/input21.txt", identity) {

  case class Food(ingredients: Set[String], allergens: Set[String])

  val entries = input.map {
    case s"$ingredients (contains $allergens)" =>
      Food(ingredients.split(' ').map(_.trim).toSet, allergens.split(',').map(_.trim).toSet)
  }

  val allergenInMultipleIngredients =
    entries
      .flatMap(e => e.allergens.map(a => a -> e.ingredients))
      .groupMapReduce(_._1)(_._2)(_ intersect _)
  val everyIngredient        = entries.flatMap(_.ingredients).toSet
  val notAllergenIngredients = everyIngredient -- allergenInMultipleIngredients.values.flatten.toSet
  entries.map(f => (notAllergenIngredients intersect f.ingredients).size).sum

  val solution1 = entries.map(f => (notAllergenIngredients intersect f.ingredients).size).sum
  println(s"solution1=$solution1")
}
