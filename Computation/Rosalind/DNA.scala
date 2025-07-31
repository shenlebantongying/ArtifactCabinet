open class Nucleotide:
  var A: Int = 0
  var C: Int = 0
  var G: Int = 0
  var T: Int = 0

  def ToString() =
    s"${A} ${C} ${G} ${T}"

@main def dna_counter() =
  var c = new Nucleotide()

  var data = scala.io.Source
    .fromFile("./data/rosalind_dna.txt")
    .mkString
    .foreach(i =>
      i match
        case 'A' => c.A += 1
        case 'C' => c.C += 1
        case 'G' => c.G += 1
        case 'T' => c.T += 1
        case _   => ()
    )

  println(c.ToString())
