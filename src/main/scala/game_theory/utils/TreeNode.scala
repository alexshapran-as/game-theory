package game_theory.utils

import game_theory.MSA

case class TreeNode(
                       id: String,
                       children: List[String],
                       parent: Option[String],
                       wins: List[List[Int]],
                       level: Int,
                       leaf: Boolean,
                       root: Boolean = false,
                       best: Boolean = false,
                       color: String = "",
                       gamer: String = ""
                   ) {
    def toMap: MSA = Map(
        "id" -> id,
        "children" -> children,
        "parent" -> parent.getOrElse(""),
        "wins" -> wins,
        "level" -> level,
        "leaf" -> leaf,
        "root" -> root,
        "best" -> best,
        "color" -> color,
        "gamer" -> gamer
    )
}