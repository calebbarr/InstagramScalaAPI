package com.antonfagerberg.instagram.responses
import com.antonfagerberg.instagram.Instagram

case class Response[T](
  data: Option[T],
  pagination: Option[Pagination],
  meta: Meta
)(implicit m:Manifest[T]) {
  
  def hasNextPage = 
    !this.pagination.isEmpty && !this.pagination.get.nextURL.isEmpty
  
  def nextPage =
    if(this.hasNextPage)
      Some(Instagram.request(pagination.get.nextURL.get))
    else Option.empty
    
  def iterator = ResponseIterator(this,retry=ResponseIterator.MAX_RETRY)
  
  def getAll = iterator
  
}

case object ResponseIterator {
  val MAX_RETRY = 5
}

case class ResponseIterator[T](response:Response[T],retry:Int=ResponseIterator.MAX_RETRY
) extends Iterator[Option[Response[T]]]{
  
  private var _next:Option[Response[T]] = Some(response)
  private var _retries = 0
  
  def next = {
    
    var _return = this._next
    
    while(_return.get.meta.code != 200 && retries < ResponseIterator.MAX_RETRY){
      _return = this._next
      _retries += 1
    }
      
    this._next = this._next.get.nextPage
    _return
  }
  
  def retries = this._retries
  
  def hasNext = !this._next.isEmpty
  
  override def isEmpty = !hasNext
  
}