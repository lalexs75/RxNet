{ SMSGsmInfo - интерфейс рассылки СМС с веб-сервисом http://gsm-inform.ru/api/

  © 2015 - Lagunov Aleksey - alexs.at.yandex.ru

  Данная библиотека является свободным программным обеспечением. Вы вправе
  распространять её и/или модифицировать в соответствии с условиями версии
  2 либо по вашему выбору с условиями более поздней версии Стандартной
  Общественной Лицензии Ограниченного Применения GNU, опубликованной Free
  Software Foundation.

  Мы распространяем эту библиотеку в надежде на то, что она будет вам
  полезной, однако НЕ ПРЕДОСТАВЛЯЕМ НА НЕЕ НИКАКИХ ГАРАНТИЙ, в том числе
  ГАРАНТИИ ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ПРИГОДНОСТИ ДЛЯ ИСПОЛЬЗОВАНИЯ
  В КОНКРЕТНЫХ ЦЕЛЯХ. Для получения более подробной информации
  ознакомьтесь со Стандартной Общественной Лицензией Ограниченного
  Применений GNU.

  Вместе с данной библиотекой вы должны были получить экземпляр
  Стандартной Общественной Лицензии Ограниченного Применения GNU. Если вы
  его не получили, сообщите об этом в Free Software Foundation, Inc., 59 Temple Place — Suite 330, Boston,
  MA 02111-1307, USA.
}

unit SMSGsmInfoStr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  sErrorAPIKeyUserIdNotSet = 'API Key or USER ID not set';
  sErrorSiteRequest        = 'Error on site request';
  sErrorUnknowApiCommand   = 'Unknow api command - "%s"';
  sErrorOnCheckStatus      = 'Error on check status';

implementation

end.

