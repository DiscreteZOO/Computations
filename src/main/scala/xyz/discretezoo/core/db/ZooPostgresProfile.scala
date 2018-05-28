package xyz.discretezoo.core.db

import com.github.tminglei.slickpg._
import slick.jdbc.PostgresProfile

trait ZooPostgresProfile extends ExPostgresProfile with PgArraySupport {

  override val api: API = new API {}

  trait API extends super.API with ArrayImplicits {
//    https://github.com/tminglei/slick-pg/blob/master/src/test/scala/com/github/tminglei/slickpg/PgArraySupportSuite.scala

    implicit val intintWitness: ElemWitness[List[Int]] = ElemWitness.AnyWitness.asInstanceOf[ElemWitness[List[Int]]]

    implicit val simpleIntIntListTypeMapper: DriverJdbcType[List[List[Int]]] = {
      new SimpleArrayJdbcType[List[Int]]("int4[]").to(_.asInstanceOf[Seq[Array[Any]]]
        .toList.map(_.toList.asInstanceOf[List[Int]]))
    }

  }

}

object ZooPostgresProfile extends ZooPostgresProfile