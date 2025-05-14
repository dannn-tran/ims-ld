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
import imsld.model.ItemPut

object ItemEditForm:
  def apply(
      signal: Signal[ItemDtoFlat],
      editedItemV: Var[Option[(Int, ItemDtoFlat)]],
      fetchedItemV: Var[Option[Item]],
      submitBus: EventBus[Unit],
      editedItemValidatedV: Var[Either[List[String], Option[(Int, ItemPut)]]]
  ) =
    val editSlugDiv =
      val inputId = "slug-input"
      div(
        label(forId := inputId, "Slug"),
        input(
          idAttr := inputId,
          controlled(
            value <-- signal.map(_.slug.getOrElse("")),
            onInput.mapToValue --> editedItemV.updater[String] { (opt, slug) =>
              opt.map { (id, item) =>
                (id, item.copy(slug = slug.some))
              }
            }
          )
        )
      )
    val editAcquireDateDiv =
      val inputId = "acquireDate-input"
      div(
        label(forId := inputId, "Acquire date"),
        input(
          idAttr := inputId,
          controlled(
            value <-- signal.map(_.acquireDate.getOrElse("")),
            onInput.mapToValue --> editedItemV.updater[String] {
              (opt, acquireDate) =>
                opt.map { (id, item) =>
                  (id, item.copy(acquireDate = acquireDate.some))
                }
            }
          )
        )
      )
    val editAcquirePriceDiv =
      val proxyV: Var[Either[String, BigDecimal]] = Var(
        Left("")
      ) // left is from HTML input, right is from signal
      div(
        input(
          cls := "acquirePriceCurrency-input",
          placeholder := "ccy",
          controlled(
            value <-- signal.map(_.acquirePriceCurrency.getOrElse("")),
            onInput.mapToValue.map(Option.apply) --> editedItemV
              .updater[Option[String]] { (item, ccy) =>
                item.map { (id, item) =>
                  (id, item.copy(acquirePriceCurrency = ccy))
                }
              }
          ),
          listId := "defaultCurrencies",
          size := 8
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
          signal.map(_.acquirePriceValue) --> {
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
          cls("error") <-- signal.map(_.acquirePriceValue.isLeft)
        )
      )
    val editAcquireSourceDiv =
      val textAreaId = "acquireSource-textARea"
      div(
        label(forId := textAreaId, "Acquire source"),
        textArea(
          idAttr := textAreaId,
          controlled(
            value <-- signal.map(_.acquireSource.getOrElse("")),
            onInput.mapToValue --> editedItemV.updater[String] {
              (opt, acquireSource) =>
                opt.map { (id, item) =>
                  (id, item.copy(acquireSource = acquireSource.some))
                }
            }
          )
        )
      )
    val editDetailsDiv =
      val textAreaId = "details-textArea"
      div(
        label(forId := textAreaId, "Details"),
        textArea(
          idAttr := textAreaId,
          controlled(
            value <-- signal.map(_.details.getOrElse("")),
            onInput.mapToValue --> editedItemV.updater[String] {
              (opt, details) =>
                opt.map { (id, item) =>
                  (id, item.copy(details = details.some))
                }
            }
          )
        )
      )
    val storageIdDiv =
      val storagesFetchStream = FetchStorages.stream
      val storageIdInputBus: EventBus[Either[String, Option[Int]]] =
        new EventBus

      val selectId = "storage-select"

      div(
        label(forId := selectId, "Storage"),
        select(
          idAttr := selectId,
          option(value := "", "Choose storage location..."),
          children <-- storagesFetchStream
            .collect { case Right(resp) =>
              resp.data
            }
            .map { lst =>
              lst.map { s => option(value := s.id.toString(), s.label) }
            },
          value <-- signal.map(_.storageId match {
            case Right(Some(id)) => id.toString()
            case _               => ""
          }),
          onInput.mapToValue.map[Either[String, Option[Int]]] { x =>
            if (x.isEmpty()) Right(None)
            else
              x.toIntOption match
                case None        => Left("Invalid storage_id: " + x)
                case Some(value) => Right(value.some)
          } --> editedItemV.updater[Either[String, Option[Int]]] {
            (opt, storageId) =>
              opt.map { (id, item) =>
                (id, item.copy(storageId = storageId))
              }
          }
        ),
        child.maybe <-- storagesFetchStream
          .collect {
            case Left(err) =>
              s"Unable to fetch storages from server. Error: $err".some
            case _ => None
          }
          .splitOption { (_, signal) =>
            p(text <-- signal, cls := "error")
          },
        child.maybe <-- storageIdInputBus.events
          .collect {
            case Left(err) =>
              s"Error encountered while updating storage_id: $err".some
            case _ => None
          }
          .splitOption { (_, signal) =>
            p(text <-- signal, cls := "error")
          }
      )

    form(
      div(
        button(
          typ := "button",
          onClick.mapTo(fetchedItemV.now()) --> editedItemV.writer
            .contramap[Option[Item]] { opt =>
              opt.map { item => (item.id, ItemDtoFlat.fromItem(item)) }
            },
          "Reset changes"
        ),
        button(
          typ := "submit",
          onClick.preventDefault.mapToUnit --> submitBus.writer,
          disabled <-- editedItemValidatedV.signal.map(_.isLeft),
          "Submit changes"
        )
      ),
      editSlugDiv,
      editAcquireDateDiv,
      editAcquirePriceDiv,
      editAcquireSourceDiv,
      editDetailsDiv,
      storageIdDiv
    )
