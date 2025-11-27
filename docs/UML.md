# SerialProcessor UML Documentation

## DataElements.pas
### TDataElement
- **Purpose**: Represents a single parsed field with framing and timeout metadata.
- **Key Attributes**: `Name`, `Prefix`, `Suffix`, `Value`, `ElementType`, `SourceElement`, `TimeRX`, `Timeout`, plus XML/timeout state.
- **Key Operations**: Constructor initializes defaults; `Process` parses input using prefix/suffix and converts to the configured type; `DataValid` checks timeout; XML getters/setters persist configuration.
- **Relationships**: May copy from another `TDataElement` via `SourceElement`.

### TDataElements
- **Purpose**: Collection that defines framing for incoming/outgoing delimited strings.
- **Key Attributes**: Shared `Prefix`, `Suffix`, `Delimiter`, `Timeout`, `LastString`, `TimeRX`, and list of `TDataElement` items.
- **Key Operations**: Find/index by name; expose concatenated field names/data; `Process` parses inbound delimited strings; timeout validation; configuration dialog; XML serialization for collection and framing settings.
- **Relationships**: Used by `TSerialPort` to parse/broadcast serial payloads; delegates parsing to contained `TDataElement` instances.

### Helper Functions/Constants
- `ElementTypeToStr`, `StrToElementType`, `ELEMENT_TYPES`, `TIMEOUT_SECONDS` provide type mapping and default timeout behavior.

## DataFields.pas
### TDataElement (DataFields variant)
- **Purpose**: Lightweight metadata model for a field definition.
- **Key Attributes**: `Name`, `ID`, `Value`, `FieldType` (runtime type info).
- **Key Operations**: Constructor sets defaults; XML getter/setter for name, ID, value, and type.

### TDataElements
- **Purpose**: Collection for field metadata definitions.
- **Key Operations**: XML serialization/deserialization of contained fields; find/index by name (case-insensitive); configuration dialog hook.

## Logger.pas
### TLogger
- **Purpose**: Thread-safe file logger for CSV and diagnostic output.
- **Key Attributes**: `Path`, `Name`, file extension, timestamp flag, `TFileStream` handle, critical section for synchronization.
- **Key Operations**: Constructor chooses directory/name and opens file; destructor frees stream; `NewFile` rolls to timestamped file; `Log` writes lines with optional timestamps, retrying with a new file on failure.
- **Relationships**: `SystemLog` lazily instantiates a global logger bound to application name; `TSerialPort` uses `TLogger` for CSV logging of readings.

## SerialPorts.pas
### TSerialPort
- **Purpose**: Wraps serial device access, parsing, and broadcasting for one port.
- **Key Attributes**: `TLazSerial` configuration, associated `TDataElements`, RX/TX string queues, read/write worker threads, optional CSV `TLogger`, port `Name`, sampling rate, notification callback.
- **Key Operations**: Lifecycle (`Start`/`Stop`) manage the serial device, threads, buffers, and logging; `DoProcessInput` feeds inbound lines to `TDataElements`; `DoBroadcast` builds outbound frames from `SourceElement` mappings and enqueues them; `LogToFile` writes CSV; setup dialogs for serial and data elements; XML serialization of port configuration and fields.
- **Relationships**: Owns `TDataElements` and forwards notifications via `OnSerialNotification`; read/write threads access `LazSerial` and queues; references `TLogger` for persistence.

### TSerialPorts
- **Purpose**: Manages multiple serial ports and shared field metadata.
- **Key Attributes**: List of `TSerialPort` objects, `AvailableFields`, shared notification handler.
- **Key Operations**: Add with duplicate-name guard; find ports/elements (supports `Port.Element` lookup); resolve `SourceElement` links from another collection; start/stop all ports; XML serialization; configuration dialog.

### TStringQueue
- **Purpose**: Thread-safe bounded FIFO for strings.
- **Key Attributes**: Backing `TStringList`, `Limit` for maximum entries.
- **Key Operations**: Enqueue with truncation of oldest entries, dequeue, flush, count.

### TSerialReadThread
- **Purpose**: Worker that samples serial input and enqueues strings.
- **Key Attributes**: Tracks `LastRead` with locking, uses `SampleRatePerSec` and `ONE_SECOND` timing.
- **Key Operations**: `Execute` loop uses `CanRead/RecvString`, logs errors, throttles skipped samples.

### TSerialWriteThread
- **Purpose**: Worker that drains TX queue and writes to the serial device.
- **Key Attributes**: Tracks `LastWrite` with locking.
- **Key Operations**: Sends strings when writable; logs failures via `SystemLog`.

### Global Constants/Utilities
- `ONE_SECOND` time conversion; `Notify` helper to relay events; `LogToFile` binds port data to CSV via `TLogger`.
