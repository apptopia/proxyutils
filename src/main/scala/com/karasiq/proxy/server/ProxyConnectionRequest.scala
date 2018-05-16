package com.karasiq.proxy.server

import java.net.InetSocketAddress

import akka.util.ByteString

import com.karasiq.networkutils.http.HttpStatus
import com.karasiq.parsers.http.HttpResponse
import com.karasiq.parsers.socks.SocksClient.SocksVersion.{SocksV4, SocksV5}
import com.karasiq.parsers.socks.SocksServer.{Codes, _}
import com.karasiq.networkutils.http.headers.HttpHeader

sealed trait ProxyConnectionRequest {
  def address: InetSocketAddress
  def scheme: String
  def headers: Seq[HttpHeader]
}

case class HttpProxyConnectionRequest(address: InetSocketAddress,
                                      headers: Seq[HttpHeader])
    extends ProxyConnectionRequest {
  override def scheme: String = "http"
}
case class HttpsProxyConnectionRequest(address: InetSocketAddress,
                                       headers: Seq[HttpHeader])
    extends ProxyConnectionRequest {
  override def scheme: String = "https"
}
case class Socks5ProxyConnectionRequest(address: InetSocketAddress)
    extends ProxyConnectionRequest {
  override def scheme: String = "socks5"
  override def headers: Seq[HttpHeader] = Seq.empty
}
case class Socks4ProxyConnectionRequest(address: InetSocketAddress)
    extends ProxyConnectionRequest {
  override def scheme: String = "socks4"
  override def headers: Seq[HttpHeader] = Seq.empty
}

object ProxyConnectionRequest {
  def successResponse(request: ProxyConnectionRequest): ByteString = {
    request match {
      case _: HttpProxyConnectionRequest ⇒
        ByteString.empty

      case _: HttpsProxyConnectionRequest ⇒
        HttpResponse((HttpStatus(200, "Connection established"), Nil))

      case _: Socks5ProxyConnectionRequest ⇒
        ConnectionStatusResponse(SocksV5, None, Codes.success(SocksV5))

      case _: Socks4ProxyConnectionRequest ⇒
        ConnectionStatusResponse(SocksV4, None, Codes.success(SocksV4))

      case _ ⇒
        throw new IllegalArgumentException(
          s"Invalid proxy connection request: $request")
    }
  }

  def failureResponse(request: ProxyConnectionRequest): ByteString = {
    request match {
      case _: HttpProxyConnectionRequest | _: HttpsProxyConnectionRequest ⇒
        HttpResponse((HttpStatus(400, "Bad Request"), Nil))

      case _: Socks5ProxyConnectionRequest ⇒
        ConnectionStatusResponse(SocksV5, None, Codes.failure(SocksV5))

      case _: Socks4ProxyConnectionRequest ⇒
        ConnectionStatusResponse(SocksV4, None, Codes.failure(SocksV4))

      case _ ⇒
        throw new IllegalArgumentException(
          s"Invalid proxy connection request: $request")
    }
  }
}
