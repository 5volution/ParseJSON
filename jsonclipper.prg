// **** **** **** Foi modificado e testado na pasta R:

#include "fivewin.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "error.ch"
#include "hbclass.ch"

// ------------------------------------------------------------
// CONSTANTES E DEFINIÇÕES
// ------------------------------------------------------------
#xtranslate HB_IsArray( <x> ) => (ValType(<x>) == "A")
#xtranslate HB_IsString( <x> ) => (ValType(<x>) == "C")
#xtranslate HB_IsNumeric( <x> ) => (ValType(<x>) == "N")
#xtranslate HB_IsLogical( <x> ) => (ValType(<x>) == "L")
#xtranslate HB_IsDate( <x> ) => (ValType(<x>) == "D")
#xtranslate HB_IsHash( <x> ) => (ValType(<x>) == "H")
#xtranslate HB_HHasKey( <h>, <k> ) => (hHasKey(<h>, <k>))

// Para simular hash arrays em Clipper
#xtranslate HASH => ARRAY
#xtranslate HB_HASH() => {}
#xtranslate HB_HASH_ADD(<h>, <k>, <v>) => (AAdd(<h>, {<k>, <v>}))
#xtranslate HB_HASH_GET(<h>, <k>) => (hGet(<h>, <k>))
#xtranslate HB_HASH_SET(<h>, <k>, <v>) => (hSet(<h>, <k>, <v>))
#xtranslate HB_HASH_KEYS(<h>) => (hKeys(<h>))
#xtranslate HB_HASH_VALUES(<h>) => (hValues(<h>))
#xtranslate HB_HASH_LEN(<h>) => (hLen(<h>))
#xtranslate HB_HASH_CLEAR(<h>) => (hClear(<h>))

// ------------------------------------------------------------
// PROGRAMA PRINCIPAL DE EXEMPLO
// ------------------------------------------------------------
FUNCTION Main()
    LOCAL cJsonExemplo
    LOCAL oJson
    LOCAL oCliente
    LOCAL aTelefones
    LOCAL i
    LOCAL aKeys
    LOCAL oTeste
    
    SET DATE BRITISH
    SET CENTURY ON
    //SET COLOR TO W+/B
    //CLS
    
    // Exemplo de JSON com hierarquia de duas dimensões
    cJsonExemplo := ;
        '{' + ;
        '  "nome": "João Silva",' + ;
        '  "idade": 30,' + ;
        '  "ativo": true,' + ;
        '  "endereco": {' + ;
        '    "rua": "Rua das Flores",' + ;
        '    "numero": 123,' + ;
        '    "cidade": "São Paulo",' + ;
        '    "estado": "SP"' + ;
        '  },' + ;
        '  "telefones": [' + ;
        '    {"tipo": "celular", "numero": "(11) 99999-9999"},' + ;
        '    {"tipo": "residencial", "numero": "(11) 3333-3333"}' + ;
        '  ]' + ;
        '}'

    ? "=== EXEMPLO 1: Classe genérica TJSONObject ==="
    
    oJson := TJSONObject():New(cJsonExemplo)

    ? "Nome completo:", oJson:GetValue("nome")
    ? "Idade:", oJson:GetValue("idade")
    ? "Cidade:", hGet(oJson:GetValue("endereco"),"cidade")
    ? "Rua:", hGet(oJson:GetValue("endereco"),"rua")
    ? "Ativo?", oJson:GetValue("ativo")
    
    ? "=== Modificando valores ==="
    oJson:SetValue("idade", 31)
    oJson:SetValue("endereco.cidade", "Campinas")
    oJson:SetValue("profissao", "Engenheiro")
    
    ? "Nova idade:", oJson:GetValue("idade")
    ? "Nova cidade:", oJson:GetValue("endereco.cidade")
    ? "Nova profissão:", oJson:GetValue("profissao")
    
    ? "=== Listando telefones ==="
    aTelefones := oJson:GetValue("telefones", {})
    
    FOR i := 1 TO Len(aTelefones)
        ? "Telefone " + AllTrim(Str(i)) + ":"
        ? "  Tipo:", hGet(aTelefones[i], "tipo")
        ? "  Número:", hGet(aTelefones[i], "numero")
    NEXT
    
    ? "=== JSON resultante (formatado) ==="
    ? oJson:ToString(2)
    
    ? "=== Salvando em arquivo ==="
    IF oJson:SaveToFile("cliente.json")
        ? "Arquivo 'cliente.json' salvo com sucesso!"
    ENDIF
    
    ? "=== EXEMPLO 2: Classe especializada TCliente ==="
    
    oCliente := TCliente():New(cJsonExemplo)
    
    ? "Nome via método específico:", oCliente:GetNome()
    ? "Cidade via método específico:", oCliente:GetCidade()
    
    oCliente:AddTelefone("comercial", "(11) 2222-2222")
    oCliente:SetNome("João da Silva")
    oCliente:SetIdade(32)
    
    ? "=== Após modificações ==="
    ? "Nome:", oCliente:GetNome()
    ? "Idade:", oCliente:GetIdade()
    
    ? "=== Todos os telefones ==="
    aTelefones := oCliente:GetTelefones()
    
    FOR i := 1 TO Len(aTelefones)
        ? hGet(aTelefones[i], "tipo") + ":", hGet(aTelefones[i], "numero")
    NEXT

    ? "=== Carregando de arquivo ==="
    oCliente2 := TCliente():New()
    
    IF oCliente2:LoadFromFile("cliente.json")
        ? "Carregado com sucesso!"
        ? "Nome:", oCliente2:GetNome()
    ELSE
        ? "Erro ao carregar arquivo!"
    ENDIF
    
    ? "=== Verificando existência de chaves ==="
    ? "Tem 'nome'?", oCliente2:HasKey("nome")
    ? "Tem 'endereco.cep'?", oCliente2:HasKey("endereco.cep")
    ? "Tem 'telefones'?", oCliente2:HasKey("telefones")
    
    ? "=== Listando todas as chaves ==="
    aKeys := oCliente2:GetKeys()
    
    ? "Chaves disponíveis:"
    FOR i := 1 TO Len(aKeys)
        ? strZero(i,2) + " - " + aKeys[i]
    NEXT
    
    ? "=== Teste de parsing JSON inválido ==="
    oTeste := TJSONObject():New('{"nome": "Teste", "idade": "abc"}')
    
    ? "Teste carregado?", IF(oTeste:IsEmpty(), "Não", "Sim")
    
    ? "=== Fim do exemplo ==="
    
    //WAIT
    
    RETURN

// ------------------------------------------------------------
// COMPILAÇÃO E EXECUÇÃO
// ------------------------------------------------------------
/*
Para compilar no Clipper 5.3:
1. Salve como jsonclipper.prg
2. Compile: clipper jsonclipper
3. Link: rtlink fi jsonclipper
4. Execute: jsonclipper

OU usando um makefile:

EXE = jsonclipper.exe
*/

static function Hex( cStr )
   return StrToHex( cStr )

static function StrToHex( cStr )
   local n, cHex := ""
   for n = 1 to Len( cStr )
      cHex += "0x" + PadL( hb_NumToHex( Asc( SubStr( cStr, n, 1 ) ) ), "0", 2 )
      if n < Len( cStr )
         cHex += ", "
      endif  
   next
   return cHex        

// ------------------------------------------------------------
// FUNÇÕES DE APOIO PARA HASH SIMULADO
// ------------------------------------------------------------
FUNCTION hHasKey(hHash, cKey)
    LOCAL nPos := AScan(hHash, {|x| Upper(x[1]) == Upper(cKey)})
    RETURN (nPos > 0)

FUNCTION hGet(hHash, cKey)
    LOCAL nPos := AScan(hHash, {|x| Upper(x[1]) == Upper(cKey)})
    RETURN IF(nPos > 0, hHash[nPos][2], NIL)

FUNCTION hSet(hHash, cKey, xValue)
    LOCAL nPos := AScan(hHash, {|x| Upper(x[1]) == Upper(cKey)})
    
    IF nPos > 0
        hHash[nPos][2] := xValue
    ELSE
        AAdd(hHash, {cKey, xValue})
    ENDIF
    RETURN .T.

FUNCTION hKeys(hHash)
    LOCAL aKeys := {}
    LOCAL i
    
    FOR i := 1 TO Len(hHash)
        AAdd(aKeys, hHash[i][1])
    NEXT
    RETURN aKeys

FUNCTION hValues(hHash)
    LOCAL aValues := {}
    LOCAL i
    
    FOR i := 1 TO Len(hHash)
        AAdd(aValues, hHash[i][2])
    NEXT
    RETURN aValues

FUNCTION hLen(hHash)
    RETURN Len(hHash)

FUNCTION hClear(hHash)
    ASize(hHash, 0)
    RETURN .T.

FUNCTION hClone(hHash)
    LOCAL hNew := {}
    LOCAL i
    
    FOR i := 1 TO Len(hHash)
        AAdd(hNew, {hHash[i][1], hHash[i][2]})
    NEXT
    RETURN hNew

// ------------------------------------------------------------
// CLASSE JSON PARSER
// ------------------------------------------------------------
CLASS TJSONParser
    HIDDEN:
        VAR cJson
        VAR nPos
        VAR nLen
        
    EXPORTED:
        METHOD New(cJson) CONSTRUCTOR
        METHOD Parse() // xResult
        METHOD ParseValue() // xValue
        METHOD ParseObject() //  hObject
        METHOD ParseArray() //  aArray
        METHOD ParseString() // cString
        METHOD ParseNumber() // nNumber
        METHOD ParseBoolean() // lBoolean
        METHOD ParseNull() // NIL
        METHOD SkipWhitespace()
        METHOD GetCurrentChar() // cChar
        METHOD Expect(cExpected)
        METHOD IsDigit(cChar) // lIsDigit
        METHOD IsHexDigit(cChar) // lIsHexDigit
ENDCLASS

METHOD New(cJson) CLASS TJSONParser
    ::cJson := cJson
    ::nPos := 1
    ::nLen := Len(cJson)
    RETURN Self

METHOD Parse() CLASS TJSONParser
    LOCAL xResult
    
    ::SkipWhitespace()
    xResult := ::ParseValue()
    ::SkipWhitespace()
    
    IF ::nPos <= ::nLen
        ? "JSON Parse Error: Extra characters at end"
        RETURN NIL
    ENDIF
    
    RETURN xResult

METHOD ParseValue() CLASS TJSONParser
    LOCAL cChar
    LOCAL xValue
    
    ::SkipWhitespace()
    
    IF ::nPos > ::nLen
        ? "JSON Parse Error: Unexpected end of JSON"
        RETURN NIL
    ENDIF
    
    cChar := SubStr(::cJson, ::nPos, 1)
    
    DO CASE
    CASE cChar == "{"
        xValue := ::ParseObject()
        
    CASE cChar == "["
        xValue := ::ParseArray()
        
    CASE cChar == '"'
        xValue := ::ParseString()
        
    CASE ::IsDigit(cChar) .OR. cChar == "-"
        xValue := ::ParseNumber()
        
    CASE Upper(cChar) == "T" .OR. Upper(cChar) == "F"
        xValue := ::ParseBoolean()
        
    CASE Upper(cChar) == "N"
        xValue := ::ParseNull()
        
    OTHERWISE
        ? "JSON Parse Error: Unexpected character at position", ::nPos
        RETURN NIL
    ENDCASE
    
    RETURN xValue

METHOD ParseObject() CLASS TJSONParser
    LOCAL hObject := {}
    LOCAL cKey
    LOCAL xValue
    LOCAL lFirst := .T.
    
    ::Expect("{")
    ::SkipWhitespace()
    
    DO WHILE ::GetCurrentChar() != "}"
        ::SkipWhitespace()
        
        IF !lFirst
            ::Expect(",")
            ::SkipWhitespace()
        ENDIF
        
        cKey := ::ParseString()
        ::SkipWhitespace()
        ::Expect(":")
        ::SkipWhitespace()
        
        xValue := ::ParseValue()
        
        hSet(hObject, cKey, xValue)
        
        lFirst := .F.
        ::SkipWhitespace()
    ENDDO
    
    ::Expect("}")
    
    RETURN hObject

METHOD ParseArray() CLASS TJSONParser
    LOCAL aArray := {}
    LOCAL xValue
    LOCAL lFirst := .T.
    
    ::Expect("[")
    ::SkipWhitespace()
    
    DO WHILE ::GetCurrentChar() != "]"
        ::SkipWhitespace()
        
        IF !lFirst
            ::Expect(",")
            ::SkipWhitespace()
        ENDIF
        
        xValue := ::ParseValue()
        AAdd(aArray, xValue)
        
        lFirst := .F.
        ::SkipWhitespace()
    ENDDO
    
    ::Expect("]")
    
    RETURN aArray

METHOD ParseString() CLASS TJSONParser
    LOCAL cString := ""
    LOCAL cChar
    LOCAL nStart
    
    ::Expect('"')
    nStart := ::nPos
    
    DO WHILE ::nPos <= ::nLen
        cChar := SubStr(::cJson, ::nPos, 1)
        
        IF cChar == '"'
            EXIT
        ELSEIF cChar == "\"
            ::nPos++
            IF ::nPos > ::nLen
                EXIT
            ENDIF
            
            cChar := SubStr(::cJson, ::nPos, 1)
            
            DO CASE
            CASE cChar == '"'
                cString += '"'
            CASE cChar == "\"
                cString += "\"
            CASE cChar == "/"
                cString += "/"
            CASE cChar == "b"
                cString += Chr(8)  // Backspace
            CASE cChar == "f"
                cString += Chr(12) // Form feed
            CASE cChar == "n"
                cString += Chr(10) // New line
            CASE cChar == "r"
                cString += Chr(13) // Carriage return
            CASE cChar == "t"
                cString += Chr(9)  // Tab
            CASE cChar == "u"
                // Unicode escape - simplificado
                IF ::nPos + 4 <= ::nLen
                    // Ignora unicode por simplicidade
                    ::nPos += 4
                    cString += "?"
                ENDIF
            ENDCASE
        ELSE
            cString += cChar
        ENDIF
        
        ::nPos++
    ENDDO
    
    ::Expect('"')
    
    RETURN cString

METHOD ParseNumber() CLASS TJSONParser
    LOCAL cNumber := ""
    LOCAL cChar
    LOCAL nValue
    
    DO WHILE ::nPos <= ::nLen
        cChar := SubStr(::cJson, ::nPos, 1)
        
        IF ::IsDigit(cChar) .OR. cChar $ "-+.eE"
            cNumber += cChar
            ::nPos++
        ELSE
            EXIT
        ENDIF
    ENDDO
    
    // Converte string para número
    IF "." $ cNumber .OR. "e" $ Lower(cNumber) .OR. "E" $ cNumber
        nValue := Val(cNumber)
    ELSE
        nValue := Val(cNumber)
    ENDIF
    
    RETURN nValue

METHOD ParseBoolean() CLASS TJSONParser
    LOCAL cValue := ""
    LOCAL lResult
    
    DO WHILE ::nPos <= ::nLen .AND. ;
             Upper(SubStr(::cJson, ::nPos, 1)) $ "TRUEFALS"
        cValue += Upper(SubStr(::cJson, ::nPos, 1))
        ::nPos++
    ENDDO
    
    IF cValue == "TRUE"
        lResult := .T.
    ELSEIF cValue == "FALSE"
        lResult := .F.
    ELSE
        ? "JSON Parse Error: Invalid boolean value"
        RETURN .F.
    ENDIF
    
    RETURN lResult

METHOD ParseNull() CLASS TJSONParser
    LOCAL cValue := ""
    
    DO WHILE ::nPos <= ::nLen .AND. ;
             Upper(SubStr(::cJson, ::nPos, 1)) $ "NULL"
        cValue += Upper(SubStr(::cJson, ::nPos, 1))
        ::nPos++
    ENDDO
    
    IF cValue != "NULL"
        ? "JSON Parse Error: Invalid null value"
    ENDIF
    
    RETURN NIL

METHOD SkipWhitespace() CLASS TJSONParser
    LOCAL cChar
    
    DO WHILE ::nPos <= ::nLen
        cChar := SubStr(::cJson, ::nPos, 1)
        
        IF cChar $ " " + Chr(9) + Chr(10) + Chr(13)
            ::nPos++
        ELSE
            EXIT
        ENDIF
    ENDDO
    
    RETURN NIL

METHOD GetCurrentChar() CLASS TJSONParser
    LOCAL cChar := ""
    
    IF ::nPos <= ::nLen
        cChar := SubStr(::cJson, ::nPos, 1)
    ENDIF
    
    RETURN cChar

METHOD Expect(cExpected) CLASS TJSONParser
    LOCAL cChar := ::GetCurrentChar()
    
    IF cChar != cExpected
        ? "JSON Parse Error: Expected '" + cExpected + "' but found '" + cChar + "'"
        RETURN .F.
    ENDIF
    
    ::nPos++
    
    RETURN .T.

METHOD IsDigit(cChar) CLASS TJSONParser
    RETURN (cChar >= "0" .AND. cChar <= "9")

METHOD IsHexDigit(cChar) CLASS TJSONParser
    LOCAL cUpper := Upper(cChar)
    RETURN (::IsDigit(cChar) .OR. ;
        (cUpper >= "A" .AND. cUpper <= "F"))

// ------------------------------------------------------------
// CLASSE PRINCIPAL JSON OBJECT
// ------------------------------------------------------------
CLASS TJSONObject
    HIDDEN:
        VAR hData
        
    EXPORTED:
        METHOD New(cJson) CONSTRUCTOR
        METHOD LoadFromString(cJson) // lSuccess
        METHOD LoadFromFile(cFileName) // lSuccess
        METHOD GetValue(cPath, xDefault) // xValue
        METHOD SetValue(cPath, xValue) // lSuccess
        METHOD Remove(cPath) // lSuccess
        METHOD HasKey(cPath) // lHasKey
        METHOD ToString(nIndent) // cJson
        METHOD ToArray() // hData
        METHOD GetKeys() // aKeys
        METHOD IsEmpty() // lEmpty
        METHOD Clear() // lSuccess
        METHOD SaveToFile(cFileName) // lSuccess
        METHOD Merge(oOtherJson) // lSuccess
        METHOD Serialize(xValue, nIndent, nLevel)
ENDCLASS

METHOD New(cJson) CLASS TJSONObject
    ::hData := {}
    
    IF HB_IsString(cJson) .AND. !Empty(cJson)
        ::LoadFromString(cJson)
    ENDIF
    
    RETURN Self

METHOD LoadFromString(cJson) CLASS TJSONObject
    LOCAL oParser
    LOCAL hJsonData
    
    IF Empty(cJson)
        ::hData := {}
        RETURN .T.
    ENDIF
    
    oParser := TJSONParser():New(cJson)
    hJsonData := oParser:Parse()
    
    IF hJsonData != NIL
        ::hData := hJsonData
        RETURN .T.
    ELSE
        ? "Erro ao carregar JSON"
        RETURN .F.
    ENDIF
    
    RETURN .F.

METHOD LoadFromFile(cFileName) CLASS TJSONObject
    LOCAL cJson := ""
    LOCAL hFile
    LOCAL nSize
    LOCAL lSuccess := .F.
    
    IF File(cFileName)
        hFile := FOpen(cFileName)
        
        IF hFile != -1
            nSize := FSeek(hFile, 0, 2)  // Vai para o final
            FSeek(hFile, 0, 0)           // Volta para início
            
            cJson := Space(nSize)
            FRead(hFile, @cJson, nSize)
            FClose(hFile)
            
            lSuccess := ::LoadFromString(cJson)
        ELSE
            ? "Erro ao abrir arquivo:", cFileName
        ENDIF
    ELSE
        ? "Arquivo não encontrado:", cFileName
    ENDIF
    
    RETURN lSuccess

METHOD GetValue(cPath, xDefault) CLASS TJSONObject
    LOCAL aPath := ParsePath(cPath)
    LOCAL hTemp
    LOCAL i
    LOCAL cKey
    
    IF Len(aPath) == 0
        RETURN xDefault
    ENDIF
    
    hTemp := ::hData
    
    FOR i := 1 TO Len(aPath)
        cKey := aPath[i]
        
        IF hHasKey(hTemp, cKey)
            IF i == Len(aPath)
                // Último elemento do caminho
                RETURN hGet(hTemp, cKey)
            ELSE
                // Continua navegando
                IF HB_IsArray(hGet(hTemp, cKey)) .AND. ;
                   hGet(hTemp, cKey) != NIL .AND. ;
                   Len(hGet(hTemp, cKey)) > 0 .AND. ;
                   HB_IsArray(hGet(hTemp, cKey)[1])
                    // É um hash/objeto
                    hTemp := hGet(hTemp, cKey)
                ELSE
                    // Caminho intermediário não é um objeto
                    RETURN xDefault
                ENDIF
            ENDIF
        ELSE
            // Chave não encontrada
            RETURN xDefault
        ENDIF
    NEXT
    
    RETURN xDefault

METHOD SetValue(cPath, xValue) CLASS TJSONObject
    LOCAL aPath := ParsePath(cPath)
    LOCAL hTemp
    LOCAL i
    LOCAL cKey
    LOCAL xTemp
    
    IF Len(aPath) == 0
        RETURN .F.
    ENDIF
    
    hTemp := ::hData
    
    FOR i := 1 TO Len(aPath)
        cKey := aPath[i]
        
        IF i == Len(aPath)
            // Último elemento - atribui o valor
            hSet(hTemp, cKey, xValue)
            RETURN .T.
        ELSE
            // Elemento intermediário
            IF !hHasKey(hTemp, cKey)
                // Cria novo nível se necessário
                hSet(hTemp, cKey, {})
            ENDIF
            
            xTemp := hGet(hTemp, cKey)
            
            IF !HB_IsArray(xTemp) .OR. xTemp == NIL
                // Converte para objeto se necessário
                hSet(hTemp, cKey, {})
            ENDIF
            
            hTemp := hGet(hTemp, cKey)
        ENDIF
    NEXT
    
    RETURN .T.

METHOD Remove(cPath) CLASS TJSONObject
    LOCAL aPath := ParsePath(cPath)
    LOCAL hTemp
    LOCAL i
    LOCAL cKey
    LOCAL nPos
    
    IF Len(aPath) == 0
        RETURN .F.
    ENDIF
    
    hTemp := ::hData
    
    FOR i := 1 TO Len(aPath)
        cKey := aPath[i]
        
        IF i == Len(aPath)
            // Último elemento - remove a chave
            nPos := AScan(hTemp, {|x| Upper(x[1]) == Upper(cKey)})
            
            IF nPos > 0
                ADel(hTemp, nPos)
                ASize(hTemp, Len(hTemp) - 1)
                RETURN .T.
            ENDIF
        ELSE
            // Elemento intermediário
            IF hHasKey(hTemp, cKey)
                hTemp := hGet(hTemp, cKey)
            ELSE
                RETURN .F.
            ENDIF
        ENDIF
    NEXT
    
    RETURN .F.

METHOD HasKey(cPath) CLASS TJSONObject
    LOCAL aPath := ParsePath(cPath)
    LOCAL hTemp
    LOCAL i
    LOCAL cKey
    
    IF Len(aPath) == 0
        RETURN .F.
    ENDIF
    
    hTemp := ::hData
    
    FOR i := 1 TO Len(aPath)
        cKey := aPath[i]
        
        IF hHasKey(hTemp, cKey)
            IF i == Len(aPath)
                RETURN .T.
            ELSE
                hTemp := hGet(hTemp, cKey)
            ENDIF
        ELSE
            RETURN .F.
        ENDIF
    NEXT
    
    RETURN .F.

METHOD ToString(nIndent) CLASS TJSONObject
    LOCAL cJson
    
    IF nIndent == NIL
        nIndent := 0
    ENDIF
    
    cJson := ::Serialize(::hData, nIndent, 0)
    
    RETURN cJson

METHOD ToArray() CLASS TJSONObject
    RETURN hClone(::hData)

METHOD GetKeys() CLASS TJSONObject
    RETURN hKeys(::hData)

METHOD IsEmpty() CLASS TJSONObject
    RETURN (Len(::hData) == 0)

METHOD Clear() CLASS TJSONObject
    ::hData := {}
    RETURN .T.

METHOD SaveToFile(cFileName) CLASS TJSONObject
    LOCAL hFile
    LOCAL cJson := ::ToString(2)
    LOCAL lSuccess := .F.
    
    hFile := FCreate(cFileName)
    
    IF hFile != -1
        FWrite(hFile, cJson)
        FClose(hFile)
        lSuccess := .T.
    ELSE
        ? "Erro ao criar arquivo:", cFileName
    ENDIF
    
    RETURN lSuccess

METHOD Merge(oOtherJson) CLASS TJSONObject
    LOCAL hOther := oOtherJson:ToArray()
    LOCAL i
    LOCAL cKey
    LOCAL xValue
    
    FOR i := 1 TO Len(hOther)
        cKey := hOther[i][1]
        xValue := hOther[i][2]
        
        ::SetValue(cKey, xValue)
    NEXT
    
    RETURN .T.

// ------------------------------------------------------------
// MÉTODOS PRIVADOS E FUNÇÕES AUXILIARES
// ------------------------------------------------------------
METHOD Serialize(xValue, nIndent, nLevel) CLASS TJSONObject
    LOCAL cResult := ""
    LOCAL i
    LOCAL cKey
    LOCAL xItem
    LOCAL cIndent
    LOCAL cLineBreak
    LOCAL cInnerIndent
    LOCAL lFirst
    
    // Configura indentação
    IF nIndent > 0
        cIndent := Space(nIndent * nLevel)
        cInnerIndent := Space(nIndent * (nLevel + 1))
        cLineBreak := Chr(13) + Chr(10)
    ELSE
        cIndent := ""
        cInnerIndent := ""
        cLineBreak := ""
    ENDIF
    
    DO CASE
    CASE HB_IsArray(xValue) .AND. Len(xValue) > 0 .AND. ;
         HB_IsArray(xValue[1]) .AND. Len(xValue[1]) == 2
        // É um objeto (hash)
        cResult += "{" + cLineBreak
        lFirst := .T.
        
        FOR i := 1 TO Len(xValue)
            cKey := xValue[i][1]
            xItem := xValue[i][2]
            
            IF !lFirst
                cResult += "," + cLineBreak
            ENDIF
            
            cResult += cInnerIndent + '"' + EscapeString(cKey) + '": ' + ;
                       ::Serialize(xItem, nIndent, nLevel + 1)
            
            lFirst := .F.
        NEXT
        
        cResult += cLineBreak + cIndent + "}"
        
    CASE HB_IsArray(xValue)
        // É um array
        cResult += "[" + cLineBreak
        lFirst := .T.
        
        FOR i := 1 TO Len(xValue)
            xItem := xValue[i]
            
            IF !lFirst
                cResult += "," + cLineBreak
            ENDIF
            
            cResult += cInnerIndent + ::Serialize(xItem, nIndent, nLevel + 1)
            
            lFirst := .F.
        NEXT
        
        cResult += cLineBreak + cIndent + "]"
        
    CASE HB_IsString(xValue)
        cResult += '"' + EscapeString(xValue) + '"'
        
    CASE HB_IsNumeric(xValue)
        IF Int(xValue) == xValue
            cResult += AllTrim(Str(xValue))
        ELSE
            cResult += AllTrim(Str(xValue, 20, 6))
        ENDIF
        
    CASE HB_IsLogical(xValue)
        cResult += IF(xValue, "true", "false")
        
    CASE HB_IsDate(xValue)
        cResult += '"' + DToC(xValue) + '"'
        
    CASE xValue == NIL
        cResult += "null"
        
    OTHERWISE
        cResult += '"' + EscapeString(AllTrim(Str(xValue))) + '"'
    ENDCASE
    
    RETURN cResult

// ------------------------------------------------------------
// FUNÇÕES AUXILIARES GLOBAIS
// ------------------------------------------------------------
FUNCTION ParsePath(cPath)
    LOCAL aPath := {}
    LOCAL nPos := 1
    LOCAL nLen := Len(cPath)
    LOCAL cChar
    LOCAL cToken := ""
    LOCAL nLevel := 0  // Para lidar com colchetes
    
    DO WHILE nPos <= nLen
        cChar := SubStr(cPath, nPos, 1)
        
        DO CASE
        CASE cChar == "."
            IF nLevel == 0
                IF !Empty(cToken)
                    AAdd(aPath, cToken)
                    cToken := ""
                ENDIF
            ELSE
                cToken += cChar
            ENDIF
            
        CASE cChar == "["
            IF !Empty(cToken)
                AAdd(aPath, cToken)
                cToken := ""
            ENDIF
            nLevel++
            
        CASE cChar == "]"
            nLevel--
            IF nLevel == 0 .AND. !Empty(cToken)
                AAdd(aPath, cToken)
                cToken := ""
            ENDIF
            
        OTHERWISE
            cToken += cChar
        ENDCASE
        
        nPos++
    ENDDO
    
    IF !Empty(cToken)
        AAdd(aPath, cToken)
    ENDIF
    
    RETURN aPath

FUNCTION EscapeString(cString)
    LOCAL cResult := ""
    LOCAL i
    LOCAL cChar

    FOR i := 1 TO Len(cString)
        IF valtype(cString) = "C"
           cChar := SubStr(cString, i, 1)
        
           DO CASE
           CASE cChar == CHR(034) // '"'
               cResult += '\'+CHR(034)

              CASE cChar == "\"
               cResult += '\\'

           CASE cChar == "/"
               cResult += '\/'

           CASE cChar == Chr(8)  // Backspace
               cResult += '\b'

           CASE cChar == Chr(12) // Form feed
               cResult += '\f'

           CASE cChar == Chr(10) // New line
               cResult += '\n'

           CASE cChar == Chr(13) // Carriage return
               cResult += '\r'

           CASE cChar == Chr(9)  // Tab
               cResult += '\t'

           OTHERWISE
               IF Asc(cChar) < 32
                   // Caracteres de controle
                   cResult += '\u' + PadL(AllTrim(Hex(Asc(cChar))), 4, "0")
               ELSE
                   cResult += cChar
               ENDIF
           ENDCASE
        ENDIF
    NEXT
    
    RETURN cResult

FUNCTION UnescapeString(cString)
    LOCAL cResult := ""
    LOCAL i := 1
    LOCAL nLen := Len(cString)
    LOCAL cChar
    LOCAL cHex
    
    DO WHILE i <= nLen
        cChar := SubStr(cString, i, 1)
        
        IF cChar == "\" .AND. i < nLen
            i++
            cChar := SubStr(cString, i, 1)
            
            DO CASE
            CASE cChar == '"'
                cResult += '"'
            CASE cChar == "\"
                cResult += "\"
            CASE cChar == "/"
                cResult += "/"
            CASE cChar == "b"
                cResult += Chr(8)
            CASE cChar == "f"
                cResult += Chr(12)
            CASE cChar == "n"
                cResult += Chr(10)
            CASE cChar == "r"
                cResult += Chr(13)
            CASE cChar == "t"
                cResult += Chr(9)
            CASE cChar == "u" .AND. i + 4 <= nLen
                cHex := SubStr(cString, i + 1, 4)
                cResult += Chr(HexToDec(cHex))
                i += 4
            OTHERWISE
                cResult += cChar
            ENDCASE
        ELSE
            cResult += cChar
        ENDIF
        
        i++
    ENDDO
    
    RETURN cResult

FUNCTION HexToDec(cHex)
    LOCAL nResult := 0
    LOCAL i
    LOCAL cDigit
    LOCAL nDigit
    
    cHex := Upper(AllTrim(cHex))
    
    FOR i := 1 TO Len(cHex)
        cDigit := SubStr(cHex, i, 1)
        
        IF IsDigit(cDigit)
            nDigit := Val(cDigit)
        ELSE
            nDigit := Asc(cDigit) - Asc("A") + 10
        ENDIF
        
        nResult := nResult * 16 + nDigit
    NEXT
    
    RETURN nResult

// ------------------------------------------------------------
// CLASSE ESPECIALIZADA PARA EXEMPLO
// ------------------------------------------------------------
CLASS TCliente FROM TJSONObject
    HIDDEN:
        VAR hData
        
    EXPORTED:
        METHOD New(cJson) CONSTRUCTOR
        METHOD GetNome() // cNome
        METHOD SetNome(cNome) // lSuccess
        METHOD GetIdade() // nIdade
        METHOD SetIdade(nIdade) // lSuccess
        METHOD GetEndereco() // hEndereco
        METHOD GetCidade() // cCidade
        METHOD GetTelefones() // aTelefones
        METHOD AddTelefone(cTipo, cNumero) // lSuccess
        METHOD GetAtivo() // lAtivo
        METHOD SetAtivo(lAtivo) // lSuccess
ENDCLASS

METHOD New(cJson) CLASS TCliente
    //SUPER:New(cJson)
    //RETURN Self // /* TJSONObject() */ ::New(cJson) // 
    ::hData := {}
    
    IF HB_IsString(cJson) .AND. !Empty(cJson)
        ::LoadFromString(cJson)
    ENDIF
    
    RETURN Self

METHOD GetNome() CLASS TCliente
    RETURN ::GetValue("nome", "")

METHOD SetNome(cNome) CLASS TCliente
    RETURN ::SetValue("nome", AllTrim(cNome))

METHOD GetIdade() CLASS TCliente
    RETURN ::GetValue("idade", 0)

METHOD SetIdade(nIdade) CLASS TCliente
    RETURN ::SetValue("idade", nIdade)

METHOD GetEndereco() CLASS TCliente
    RETURN ::GetValue("endereco", {})

METHOD GetCidade() CLASS TCliente
    LOCAL hEndereco := ::GetEndereco()
    LOCAL xCidade

    IF HB_IsArray(hEndereco) .AND. Len(hEndereco) > 0
        xCidade := hGet(hEndereco, "cidade")
        RETURN IF(xCidade != NIL, xCidade, "")
    ENDIF

    RETURN ""

METHOD GetTelefones() CLASS TCliente
    RETURN ::GetValue("telefones", {})

METHOD AddTelefone(cTipo, cNumero) CLASS TCliente
    LOCAL aTelefones := ::GetTelefones()
    LOCAL hTelefone := {}
    
    hSet(hTelefone, "tipo", cTipo)
    hSet(hTelefone, "numero", cNumero)
    
    AAdd(aTelefones, hTelefone)
    ::SetValue("telefones", aTelefones)
    
    RETURN .T.

METHOD GetAtivo() CLASS TCliente
    RETURN ::GetValue("ativo", .T.)

METHOD SetAtivo(lAtivo) CLASS TCliente
    RETURN ::SetValue("ativo", lAtivo)

