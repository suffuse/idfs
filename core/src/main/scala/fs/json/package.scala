package sfs
package fs

import play.api.libs.{json => pjson}

package object json {
  type Writes[-A] = pjson.Writes[A]
  type Reads[A]   = pjson.Reads[A]
  def Json        = pjson.Json
}
