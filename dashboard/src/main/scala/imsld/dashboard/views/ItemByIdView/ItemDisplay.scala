package imsld.dashboard.views.ItemByIdView

import com.raquo.airstream.core.Signal
import imsld.model.Item
import com.raquo.laminar.api.L.*
import imsld.dashboard.utils.ItemDtoFlat

object ItemDisplay:
  def apply(
      signal: Signal[Item],
      editedItemWriter: Observer[Option[(Int, ItemDtoFlat)]]
  ) =
    val tableData = List(
      ("ID", signal.map(_.id.toString())),
      ("Slug", signal.map(_.slug.getOrElse(""))),
      ("Label", signal.map(_.label.getOrElse(""))),
      ("Acquire date", signal.map(_.acquireDate.getOrElse(""))),
      (
        "Acquire price",
        signal.map(_.acquirePrice.fold("") { p => s"${p.value} ${p.currency}" })
      ),
      ("Acquire source", signal.map(_.acquireSource.getOrElse(""))),
      ("Note", signal.map(_.note.getOrElse(""))),
      (
        "Storage",
        signal.map(_.storage.fold("") { s =>
          s.label.fold(s"ID ${s.id}") { str => s"$str (ID ${s.id})" }
        })
      )
    )

    val clickBus = new EventBus[Unit]

    div(
      table(
        idAttr := "item-display-table",
        tableData.map { (headerText, valueS) =>
          tr(
            th(headerText),
            td(text <-- valueS)
          )
        }
      ),
      button(
        typ := "button",
        "Edit",
        onClick.mapToUnit --> clickBus
      ),
      clickBus.events.withCurrentValueOf(signal).map { item =>
        Option((item.id, ItemDtoFlat.fromItem(item)))
      } --> editedItemWriter
    )
