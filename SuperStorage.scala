package mmr.workshop

import scala.collection.mutable.Map
import xml.{Node, XML}

/**
 * Created by IntelliJ IDEA.
 * User: mmr
 * Date: 14.04.12
 * Time: 15:30
 * To change this template use File | Settings | File Templates.
 */
case class Item(id: Long, votes: Long, content: String) {
  def toXml = <item>
    <id>
      {id}
    </id>
    <votes>
      {votes}
    </votes>
    <content>
      {content}
    </content>
  </item>

}

class SuperStorage(fileName: String) {

  var mapToStore: Map[Long, Item] = Map.empty


  def add(objectToSave: Item) = {
    mapToStore += ((objectToSave.id, objectToSave))
    mapToStore
  }

  def toXml =
    <storage>
      {mapToStore.values.map(_.toXml)}
    </storage>

  def load = {
    mapToStore = fromXml(XML.loadFile(fileName))
    mapToStore
  }

  def fromXml(xml: Node) = {
    var temp: Map[Long, Item] = Map.empty
    for (xmlItem <- xml \\ "item") {
      val item = Item((xmlItem \ "id").text.toLong,
        (xmlItem \ "votes").text.toLong,
        (xmlItem \ "content").text)
      temp += ((item.id, item))
      println(temp)
    }
    temp
  }


  def store {
    XML.save(fileName, this.toXml)
  }


}