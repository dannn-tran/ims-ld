package imsld.dashboard.views.ItemByIdView

import com.raquo.airstream.core.Signal
import imsld.model.Item
import com.raquo.laminar.api.L.*

object ItemDisplay:
  def apply(signal: Signal[Item]) =
    div(
      p(text <-- signal.map { item =>
        s"Slug: ${item.slug.getOrElse("")}"
      }),
      p(text <-- signal.map { item =>
        s"Acquire date : ${item.acquireDate.getOrElse("")}"
      }),
      p(text <-- signal.map { item =>
        s"Acquire price: ${item.acquirePrice
            .fold("") { p => s"${p.value} ${p.currency}" }}"
      }),
      p(text <-- signal.map { item =>
        s"Acquire source: ${item.acquireSource.getOrElse("")}"
      }),
      p(text <-- signal.map { item =>
        s"Details: ${item.details.getOrElse("")}"
      }),
      p(text <-- signal.map { item =>
        s"Storage: ${item.storage.flatMap(_.label)}"
      })
    )
