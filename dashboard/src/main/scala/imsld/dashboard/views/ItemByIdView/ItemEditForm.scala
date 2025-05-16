package imsld.dashboard.views.ItemByIdView

import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L.*
import imsld.dashboard.utils.ItemDtoFlat
import cats.syntax.all.*
import imsld.dashboard.constants.CCY_OPTIONS
import scala.util.Try
import imsld.dashboard.utils.BigDecimalFormatter
import imsld.dashboard.utils.FetchStorages
import imsld.model.Item

object ItemEditForm:
  def apply(
      signal: Signal[(Int, ItemDtoFlat)],
      editedItemV: Var[Option[(Int, ItemDtoFlat)]],
      fetchedItemS: Signal[Option[Item]],
      submitBus: EventBus[Unit],
      submitDisabledS: Signal[Boolean],
      inputDisabledS: Signal[Boolean]
  ) =
    val idTr =
      tr(
        th("ID"),
        td(text <-- signal.map { (id, _) => id.toString() })
      )
    val slugTr =
      val inputId = "slug-input"
      tr(
        th(label(forId := inputId, "Slug")),
        td(
          input(
            idAttr := inputId,
            controlled(
              value <-- signal.map { (_, item) => item.slug.getOrElse("") },
              onInput.mapToValue --> editedItemV.updater[String] {
                (opt, slug) =>
                  opt.map { (id, item) =>
                    (id, item.copy(slug = slug.some))
                  }
              }
            ),
            disabled <-- inputDisabledS
          )
        )
      )
    val acquireDateTd =
      val inputId = "acquireDate-input"
      tr(
        th(label(forId := inputId, "Acquire date")),
        td(
          input(
            idAttr := inputId,
            controlled(
              value <-- signal.map { (_, item) =>
                item.acquireDate.getOrElse("")
              },
              onInput.mapToValue --> editedItemV.updater[String] {
                (opt, acquireDate) =>
                  opt.map { (id, item) =>
                    (id, item.copy(acquireDate = acquireDate.some))
                  }
              }
            ),
            disabled <-- inputDisabledS
          )
        )
      )
    val acquirePriceTr =
      val proxyV: Var[Either[String, BigDecimal]] = Var(
        Left("")
      ) // left is from HTML input, right is from signal
      tr(
        th("Acquire price"),
        td(
          input(
            cls := "acquirePriceCurrency-input",
            placeholder := "ccy",
            controlled(
              value <-- signal.map { (_, item) =>
                item.acquirePriceCurrency.getOrElse("")
              },
              onInput.mapToValue.map(Option.apply) --> editedItemV
                .updater[Option[String]] { (item, ccy) =>
                  item.map { (id, item) =>
                    (id, item.copy(acquirePriceCurrency = ccy))
                  }
                }
            ),
            listId := "defaultCurrencies",
            size := 8,
            disabled <-- inputDisabledS
          ),
          dataList(
            idAttr := "defaultCurrencies",
            CCY_OPTIONS.map { ccy =>
              option(value := ccy)
            }
          ),
          input(
            placeholder := "value",
            controlled(
              value <-- proxyV.signal.map(
                _.fold(identity, BigDecimalFormatter.format)
              ),
              onInput.mapToValue --> proxyV.writer
                .contramap[String](Left.apply)
            ),
            signal.map { (_, item) => item.acquirePriceValue } --> {
              case Right(Some(bd)) =>
                proxyV.writer.onNext(bd.asRight)
              case _ => ()
            },
            proxyV.signal --> {
              case Left(str) =>
                val v =
                  if (str.isBlank()) Right(None)
                  else
                    Try(BigDecimal(str.filter(_ != ','))).fold(
                      _ => Left(s"Invalid decimal $str"),
                      bd => Right(bd.some)
                    )
                editedItemV
                  .updater[Either[String, Option[BigDecimal]]] { (item, v) =>
                    item.map { (id, item) =>
                      (id, item.copy(acquirePriceValue = v))
                    }
                  }
                  .onNext(v)
              case _ => ()
            },
            size := 16,
            cls("error") <-- signal.map { (_, item) =>
              item.acquirePriceValue.isLeft
            },
            disabled <-- inputDisabledS
          )
        )
      )
    val acquireSourceTr =
      val textAreaId = "acquireSource-textARea"
      tr(
        th(label(forId := textAreaId, "Acquire source")),
        td(
          textArea(
            idAttr := textAreaId,
            controlled(
              value <-- signal.map { (_, item) =>
                item.acquireSource.getOrElse("")
              },
              onInput.mapToValue --> editedItemV.updater[String] {
                (opt, acquireSource) =>
                  opt.map { (id, item) =>
                    (id, item.copy(acquireSource = acquireSource.some))
                  }
              }
            ),
            disabled <-- inputDisabledS
          )
        )
      )
    val noteTr =
      val textAreaId = "details-textArea"
      tr(
        th(label(forId := textAreaId, "Details")),
        td(
          textArea(
            idAttr := textAreaId,
            controlled(
              value <-- signal.map { (_, item) => item.details.getOrElse("") },
              onInput.mapToValue --> editedItemV.updater[String] {
                (opt, details) =>
                  opt.map { (id, item) =>
                    (id, item.copy(details = details.some))
                  }
              }
            ),
            disabled <-- inputDisabledS
          )
        )
      )
    val storageIdTr =
      val storageIdS = signal.map { (_, item) =>
        item.storageId
      }
      val storageIdUpdater = editedItemV.updater[Option[Int]] {
        (opt, storageId) =>
          opt.map { (id, item) =>
            (id, item.copy(storageId = storageId))
          }
      }
      val storagesFetchStream = FetchStorages.stream

      val selectId = "storage-select"

      tr(
        th(label(forId := selectId, "Storage")),
        td(
          select(
            idAttr := selectId,
            placeholder := "Choose a storage location",
            option(
              value := "",
              "None",
              selected <-- storageIdS.map(_.isEmpty),
              onClick.mapTo(None) --> storageIdUpdater
            ),
            children <-- storagesFetchStream
              .collect { case Right(resp) =>
                resp.data
              }
              .map { lst =>
                lst.map { s =>
                  option(
                    value := s.id.toString(),
                    s.label,
                    selected <-- storageIdS.map(_.contains(s.id)),
                    onClick.mapTo(s.id.some) --> storageIdUpdater
                  )
                }
              },
            disabled <-- inputDisabledS
          ),
          child.maybe <-- storagesFetchStream
            .collect {
              case Left(err) =>
                s"Unable to fetch storages from server. Error: $err".some
              case _ => None
            }
            .splitOption { (_, signal) =>
              p(text <-- signal, cls := "error")
            }
        )
      )

    val resetBus: EventBus[Unit] = new EventBus

    form(
      table(
        idAttr := "item-edit-table",
        idTr,
        slugTr,
        acquireDateTd,
        acquirePriceTr,
        acquireSourceTr,
        noteTr,
        storageIdTr
      ),
      div(
        idAttr := "item-edit-btn-grp",
        div(
          cls := "item-edit-btn-row",
          button(
            typ := "button",
            onClick.mapToUnit --> resetBus.writer,
            "Reset changes"
          ),
          resetBus.events.withCurrentValueOf(fetchedItemS).map { opt =>
            opt.map { item => (item.id, ItemDtoFlat.fromItem(item)) }
          } --> editedItemV.writer,
          button(
            typ := "submit",
            onClick.preventDefault.mapToUnit --> submitBus.writer,
            disabled <-- submitDisabledS,
            "Submit changes"
          )
        ),
        div(
          cls := "item-edit-btn-row",
          button(
            typ := "button",
            "Back to view",
            onClick.mapTo(None) --> editedItemV.writer
          )
        )
      )
    )
