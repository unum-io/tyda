package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.analysis.SimpleAnalyzer
import org.apache.spark.sql.catalyst.expressions.Alias
import org.apache.spark.sql.catalyst.expressions.AttributeSeq
import org.apache.spark.sql.catalyst.expressions.BindReferences
import org.apache.spark.sql.catalyst.expressions.Expression
import org.apache.spark.sql.catalyst.expressions.Nondeterministic
import org.apache.spark.sql.catalyst.optimizer.ReplaceExpressions
import org.apache.spark.sql.catalyst.plans.logical.LocalRelation
import org.apache.spark.sql.catalyst.plans.logical.Project
import org.apache.spark.sql.catalyst.types.DataTypeUtils
import org.apache.spark.sql.types.StructType

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Expr
import com.choreograph.tyda.spark.CodecToCatalystType.catalystType
import com.choreograph.tyda.testsuites.ExprEvaluationSuite

/* This suite evaluates expressions without doing creating a full plan.
 * The hope is that this leads to much faster test for the ExprOnSpark logic. */
class ExprEvaluationSuiteSpark extends ExprEvaluationSuite, SharedSparkSession {
  private def createDeserializer[T: Codec]: Any => T = {
    // TYPE SAFETY: Is only used when spark represents the type as an InternalRow.
    val exprDeserializer = CodecToEncoder
      .convertInternal[T]
      .resolveAndBind()
      .createDeserializer()
      .apply
      .asInstanceOf[Any => T]

    Codec[T] match {
      case _: Codec.Product[T] | _: Codec.Sum[T, ?] => exprDeserializer
      /* Internally in Spark expr evaluation non product types are not wrapped in an InternalRow, but they
       * need to be for the deserializer. */
      case _ => value => exprDeserializer(InternalRow(value))
    }
  }

  /** This is based on the resolveAndBind for the ExpressionEncoders [0]. We
    * need to preform this step to make our Spark expression be resolved (e.g
    * access by name have been replaced by access by index) and bound to the
    * schema before we can evaluate it.
    *
    * [0]
    * https://github.com/apache/spark/blob/7c29c664cdc9321205a98a14858aaf8daaa19db2/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/encoders/ExpressionEncoder.scala#L348
    */
  private def resolveAndBind(expr: Expression, schema: StructType): Expression = {
    val attrs = DataTypeUtils.toAttributes(schema)
    val analyzer = SimpleAnalyzer
    val dummyPlan = Project(Seq(Alias(expr, "result")()), LocalRelation(attrs))
    val analyzedPlan = analyzer.execute(dummyPlan)
    analyzer.checkAnalysis(analyzedPlan)
    /* If we do not apply ReplaceExpressions we run into assert failures on this assert
     * https://github.com/apache/spark/blob/b5ed5220267d639f0fae73d1fb1b4de7e84adecc/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/expressions/Expression.scala#L463-L465 */
    val replacedPlan = ReplaceExpressions(analyzedPlan)
    val resolvedExpr = replacedPlan.expressions.head
    BindReferences.bindReference(resolvedExpr, attrs)
  }

  def evaluator[From: Codec, To](compiled: CompiledExpr[From, To]): From => To = {
    val encoder = CodecToEncoder.convertInternal[From]
    val serializer = encoder.createSerializer()
    val column = ExprOnSpark.unresolved[From](compiled)
    val sparkExpr = resolveAndBind(columnToExpression(column), encoder.schema)
    /* Based on
     * https://github.com/apache/spark/blob/v4.1.1/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/expressions/ExpressionsEvaluator.scala#L48
     * Without initialization Nondeterministic expressions will throw an exception during eval. */
    sparkExpr.foreach {
      case n: Nondeterministic => n.initialize(0)
      case _ =>
    }
    val deserializer = createDeserializer[To](using compiled.codec)
    val expectedType = catalystType(compiled.codec)
    assert(
      withAllNullable(sparkExpr.dataType) == withAllNullable(expectedType),
      s"Got\n${sparkExpr.dataType} but expected\n${expectedType}"
    )
    from => deserializer(sparkExpr.eval(serializer(from)))
  }

  override def evaluate[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): Seq[To] = {
    val eval = evaluator(CompiledExpr(expr))
    values.map(eval)
  }

  override def explain[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): String = {
    val c: Column = ExprOnSpark.unresolved[From](CompiledExpr(expr))
    c.toString
  }
}
