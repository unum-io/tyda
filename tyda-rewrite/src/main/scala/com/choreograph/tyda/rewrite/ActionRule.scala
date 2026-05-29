package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Dataset

/** A rewrite rule for [[com.choreograph.tyda.Dataset.Action]] nodes.
  *
  * Since actions are always root nodes this rule will just be applied to the
  * root node of a tree.
  */
private[tyda] trait ActionRule {
  def apply(node: Dataset.Action): Dataset.Action
}
