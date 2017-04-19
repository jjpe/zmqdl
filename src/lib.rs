extern crate libc;
extern crate libloading as lib;
extern crate zmq;
extern crate zmq_sys;

use libc::{c_char, c_int, c_void, size_t};
use std::ffi::CString;
use std::io;
use std::mem;
use std::path::{Path, PathBuf};
use std::time::Duration;
#[cfg(target_os = "linux")] use std::process;
pub use zmq::{Constants, SocketType};
use zmq_sys::zmq_msg_t;

/// This function is inspired by, but different from, the code in zmq_sys. The
/// reason is that it panics when it doesn't know what to do, which is a problem
/// as it means an interrupt can crash any consumer of this library. This fn
/// fixes that deficiency by returning `None` instead of panicking.
pub fn errno_to_error() -> Option<zmq::Error> {
    match unsafe { zmq_sys::zmq_errno() } {
        zmq_sys::errno::EACCES           => Some(zmq::Error::EACCES),
        zmq_sys::errno::EADDRINUSE       => Some(zmq::Error::EADDRINUSE),
        zmq_sys::errno::EAGAIN           => Some(zmq::Error::EAGAIN),
        zmq_sys::errno::EBUSY            => Some(zmq::Error::EBUSY),
        zmq_sys::errno::ECONNREFUSED     => Some(zmq::Error::ECONNREFUSED),
        zmq_sys::errno::EFAULT           => Some(zmq::Error::EFAULT),
        zmq_sys::errno::EHOSTUNREACH     => Some(zmq::Error::EHOSTUNREACH),
        zmq_sys::errno::EINPROGRESS      => Some(zmq::Error::EINPROGRESS),
        zmq_sys::errno::EINVAL           => Some(zmq::Error::EINVAL),
        zmq_sys::errno::EMFILE           => Some(zmq::Error::EMFILE),
        zmq_sys::errno::EMSGSIZE         => Some(zmq::Error::EMSGSIZE),
        zmq_sys::errno::ENAMETOOLONG     => Some(zmq::Error::ENAMETOOLONG),
        zmq_sys::errno::ENODEV           => Some(zmq::Error::ENODEV),
        zmq_sys::errno::ENOENT           => Some(zmq::Error::ENOENT),
        zmq_sys::errno::ENOMEM           => Some(zmq::Error::ENOMEM),
        zmq_sys::errno::ENOTCONN         => Some(zmq::Error::ENOTCONN),
        zmq_sys::errno::ENOTSOCK         => Some(zmq::Error::ENOTSOCK),
        zmq_sys::errno::EPROTO           => Some(zmq::Error::EPROTO),
        zmq_sys::errno::EPROTONOSUPPORT  => Some(zmq::Error::EPROTONOSUPPORT),
        zmq_sys::errno::ENOTSUP          => Some(zmq::Error::ENOTSUP),
        zmq_sys::errno::ENOBUFS          => Some(zmq::Error::ENOBUFS),
        zmq_sys::errno::ENETDOWN         => Some(zmq::Error::ENETDOWN),
        zmq_sys::errno::EADDRNOTAVAIL    => Some(zmq::Error::EADDRNOTAVAIL),
        156384714                        => Some(zmq::Error::EPROTONOSUPPORT),
        156384715                        => Some(zmq::Error::ENOBUFS),
        156384716                        => Some(zmq::Error::ENETDOWN),
        156384717                        => Some(zmq::Error::EADDRINUSE),
        156384718                        => Some(zmq::Error::EADDRNOTAVAIL),
        156384719                        => Some(zmq::Error::ECONNREFUSED),
        156384720                        => Some(zmq::Error::EINPROGRESS),
        156384721                        => Some(zmq::Error::ENOTSOCK),
        156384763                        => Some(zmq::Error::EFSM),
        156384764                        => Some(zmq::Error::ENOCOMPATPROTO),
        156384765                        => Some(zmq::Error::ETERM),
        156384766                        => Some(zmq::Error::EMTHREAD),
        _ => None,
    }
}

macro_rules! cfn {
    (fn $name:ident($($args:ident : $argtypes:ty),*) -> $ret:ty,
     in $lib:expr) => {{
         let name_bytes = stringify!($name).as_bytes();
         type FnSig = unsafe extern fn ($($argtypes),*) -> $ret;
         unsafe { $lib.get(name_bytes)? as lib::Symbol<FnSig> }
     }};
}

/******************************************************************************/
/*                              ZmqCtx                                        */
/******************************************************************************/
pub type ZmqCtxResult<T> = Result<T, ZmqCtxErr>;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ZmqCtxErr {
    FailedToTerminate { return_code: i64 },

    IoNotFound(Option<String>),
    IoPermissionDenied(Option<String>),
    IoConnectionRefused(Option<String>),
    IoConnectionReset(Option<String>),
    IoConnectionAborted(Option<String>),
    IoNotConnected(Option<String>),
    IoAddrInUse(Option<String>),
    IoAddrNotAvailable(Option<String>),
    IoBrokenPipe(Option<String>),
    IoAlreadyExists(Option<String>),
    IoWouldBlock(Option<String>),
    IoInvalidInput(Option<String>),
    IoInvalidData(Option<String>),
    IoTimedOut(Option<String>),
    IoWriteZero(Option<String>),
    IoInterrupted(Option<String>),
    IoOther(Option<String>),
    IoUnexpectedEof(Option<String>),
}

impl From<io::Error> for ZmqCtxErr {
    fn from(ioe: io::Error) -> ZmqCtxErr {
        use io::ErrorKind;
        use std::error::Error;
        let cause = ioe.cause().map(|err: &Error| format!("{}", err));
        match ioe.kind() {
            ErrorKind::NotFound => ZmqCtxErr::IoNotFound(cause),
            ErrorKind::PermissionDenied => ZmqCtxErr::IoPermissionDenied(cause),
            ErrorKind::ConnectionRefused => ZmqCtxErr::IoConnectionRefused(cause),
            ErrorKind::ConnectionReset => ZmqCtxErr::IoConnectionReset(cause),
            ErrorKind::ConnectionAborted => ZmqCtxErr::IoConnectionAborted(cause),
            ErrorKind::NotConnected => ZmqCtxErr::IoNotConnected(cause),
            ErrorKind::AddrInUse => ZmqCtxErr::IoAddrInUse(cause),
            ErrorKind::AddrNotAvailable => ZmqCtxErr::IoAddrNotAvailable(cause),
            ErrorKind::BrokenPipe => ZmqCtxErr::IoBrokenPipe(cause),
            ErrorKind::AlreadyExists => ZmqCtxErr::IoAlreadyExists(cause),
            ErrorKind::WouldBlock => ZmqCtxErr::IoWouldBlock(cause),
            ErrorKind::InvalidInput => ZmqCtxErr::IoInvalidInput(cause),
            ErrorKind::InvalidData => ZmqCtxErr::IoInvalidData(cause),
            ErrorKind::TimedOut => ZmqCtxErr::IoTimedOut(cause),
            ErrorKind::WriteZero => ZmqCtxErr::IoWriteZero(cause),
            ErrorKind::Interrupted => ZmqCtxErr::IoInterrupted(cause),
            ErrorKind::Other => ZmqCtxErr::IoOther(cause),
            ErrorKind::UnexpectedEof => ZmqCtxErr::IoUnexpectedEof(cause),
            _ => unimplemented!(),
        }
    }
}


pub struct ZmqCtx<'z> { ptr: *mut c_void, lib: &'z ZmqLib, }

impl<'z> ZmqCtx<'z> {
    /// See [zmq-socket](http://api.zeromq.org/4-1:zmq-socket)
    pub fn new_socket(&self, stype: SocketType) -> ZmqCtxResult<ZmqSocket> {
        let func = cfn! {
            fn zmq_socket(ctx: *mut c_void, stype: c_int) -> *mut c_void,
            in self.lib.lib
        };
        Ok(ZmqSocket {
            ptr: unsafe { func(self.ptr, stype as c_int) },
            zmqlib: &self.lib,
            rx_buf: zmq_msg_t::default()
        })
    }

    pub fn term(&self) -> ZmqCtxResult<()> {
        let term = cfn! {
            fn zmq_term(context: *mut c_void) -> c_int,
            in self.lib.lib
        };
        match unsafe { term(self.ptr) } {
            0 => Ok(()),
            rc => Err(ZmqCtxErr::FailedToTerminate { return_code: rc as i64 }),
        }
    }
}

impl<'z> Drop for ZmqCtx<'z> {
    fn drop(&mut self) {
        self.term().expect("Could not terminate context");
    }
}

/******************************************************************************/
/*                              ZmqSocket                                     */
/******************************************************************************/
pub type SockResult<T> = Result<T, SockErr>;

#[derive(Clone, Debug, PartialEq)]
pub enum SockErr {
    FailedToSetOption(Constants),
    FailedToGetOption(Constants),
    FailedToConnect { address: String },
    FailedToDisconnect { address: String },
    FailedToBind { address: String },
    FailedToReceive,
    FailedToSend,
    FailedToCloseSocket,
    MessageTruncated,

    Interrupted,
    TemporarilyUnavailable,

    IoNotFound(Option<String>),
    IoPermissionDenied(Option<String>),
    IoConnectionRefused(Option<String>),
    IoConnectionReset(Option<String>),
    IoConnectionAborted(Option<String>),
    IoNotConnected(Option<String>),
    IoAddrInUse(Option<String>),
    IoAddrNotAvailable(Option<String>),
    IoBrokenPipe(Option<String>),
    IoAlreadyExists(Option<String>),
    IoWouldBlock(Option<String>),
    IoInvalidInput(Option<String>),
    IoInvalidData(Option<String>),
    IoTimedOut(Option<String>),
    IoWriteZero(Option<String>),
    IoInterrupted(Option<String>),
    IoOther(Option<String>),
    IoUnexpectedEof(Option<String>),
}

impl From<io::Error> for SockErr {
    fn from(ioe: io::Error) -> SockErr {
        use io::ErrorKind;
        use std::error::Error;
        let cause = ioe.cause().map(|err: &Error| format!("{}", err));
        match ioe.kind() {
            ErrorKind::NotFound => SockErr::IoNotFound(cause),
            ErrorKind::PermissionDenied => SockErr::IoPermissionDenied(cause),
            ErrorKind::ConnectionRefused => SockErr::IoConnectionRefused(cause),
            ErrorKind::ConnectionReset => SockErr::IoConnectionReset(cause),
            ErrorKind::ConnectionAborted => SockErr::IoConnectionAborted(cause),
            ErrorKind::NotConnected => SockErr::IoNotConnected(cause),
            ErrorKind::AddrInUse => SockErr::IoAddrInUse(cause),
            ErrorKind::AddrNotAvailable => SockErr::IoAddrNotAvailable(cause),
            ErrorKind::BrokenPipe => SockErr::IoBrokenPipe(cause),
            ErrorKind::AlreadyExists => SockErr::IoAlreadyExists(cause),
            ErrorKind::WouldBlock => SockErr::IoWouldBlock(cause),
            ErrorKind::InvalidInput => SockErr::IoInvalidInput(cause),
            ErrorKind::InvalidData => SockErr::IoInvalidData(cause),
            ErrorKind::TimedOut => SockErr::IoTimedOut(cause),
            ErrorKind::WriteZero => SockErr::IoWriteZero(cause),
            ErrorKind::Interrupted => SockErr::IoInterrupted(cause),
            ErrorKind::Other => SockErr::IoOther(cause),
            ErrorKind::UnexpectedEof => SockErr::IoUnexpectedEof(cause),
            _ => unimplemented!(),
        }
    }
}


pub struct ZmqSocket<'z> {
    ptr: *mut c_void,
    zmqlib: &'z ZmqLib,
    rx_buf: zmq_msg_t,
}

impl<'z> ZmqSocket<'z> {
    fn set_option(&self, name: Constants, value: *const c_void, len: size_t)
                  -> SockResult<()> {
        let func = cfn! {
            fn zmq_setsockopt(socket: *mut c_void,
                              option_name: c_int,
                              option_value: *const c_void,
                              option_len: size_t) -> c_int,
            in self.zmqlib.lib
        };
        match unsafe { func(self.ptr, name as c_int, value, len) } {
            0 => Ok(()),
            _ => Err(SockErr::FailedToSetOption(name)),
        }
    }

    fn set_int_option(&self, arg: Constants, value: c_int) -> SockResult<()> {
        let ptr = &value as *const c_int as *const c_void;
        self.set_option(arg, ptr, mem::size_of::<c_int>())
    }

    fn set_slice_option<T>(&self, arg: Constants, value: &[T])
                           -> SockResult<()> {
        let len = value.len();
        let ptr = value.as_ptr() as *const c_void;
        self.set_option(arg, ptr, len * mem::size_of::<T>() as size_t)
    }

    #[allow(unused)]
    fn set_mut_slice_option<T>(&self, arg: Constants, value: &mut [T])
                               -> SockResult<()> {
        let len = value.len();
        let ptr = value.as_mut_ptr() as *mut c_void;
        self.set_option(arg, ptr, len * mem::size_of::<T>() as size_t)
    }

    pub fn set_linger_period(&self, millis: c_int) -> SockResult<()> {
        self.set_int_option(Constants::ZMQ_LINGER, millis)
    }

    pub fn set_recv_timeout(&self, millis: c_int) -> SockResult<()> {
        self.set_int_option(Constants::ZMQ_RCVTIMEO, millis)
    }

    pub fn set_send_timeout(&self, millis: c_int) -> SockResult<()> {
        self.set_int_option(Constants::ZMQ_SNDTIMEO, millis)
    }

    pub fn subscribe(&self, filter: &[u8]) -> SockResult<()> {
        self.set_slice_option(Constants::ZMQ_SUBSCRIBE, filter)
    }

    /// See [zmq-connect](http://api.zeromq.org/4-1:zmq-connect)
    pub fn connect(&self, addr: &str) -> SockResult<()> {
        let func = cfn! {
            fn zmq_connect(socket: *mut c_void, addr: *const c_char) -> c_int,
            in self.zmqlib.lib
        };
        let cstr = CString::new(addr).unwrap();
        let addr_ptr = cstr.as_ptr() as *const c_char;
        match unsafe { func(self.ptr, addr_ptr) } {
            0 => Ok(()),
            _ => Err(SockErr::FailedToConnect { address: String::from(addr) }),
        }
    }

    /// See [zmq-disconnect](http://api.zeromq.org/4-1:zmq-disconnect)
    pub fn disconnect(&self, addr: &str) -> SockResult<()> {
        let func = cfn! {
            fn zmq_disconnect(socket: *mut c_void, addr: *const c_char) -> c_int,
            in self.zmqlib.lib
        };
        let cstr = CString::new(addr).unwrap();
        let addr_ptr = cstr.as_ptr() as *const c_char;
        match unsafe { func(self.ptr, addr_ptr) } {
            0 => Ok(()),
            _ => Err(SockErr::FailedToDisconnect { address: String::from(addr) }),
        }
    }

    /// See [zmq-bind](http://api.zeromq.org/4-1:zmq-bind)
    pub fn bind(&self, addr: &str) -> SockResult<()> {
        let func = cfn! {
            fn zmq_bind(socket: *mut c_void, addr: *const c_char) -> c_int,
            in self.zmqlib.lib
        };
        let addr_ptr = CString::new(addr).unwrap().into_raw();
        match unsafe { func(self.ptr, addr_ptr) } {
            0 => Ok(()),
            _ => Err(SockErr::FailedToBind { address: String::from(addr) }),
        }
    }

    /// See [zmq_recv](http://api.zeromq.org/4-1:zmq_recv)
    pub fn receive<'b>(&self, buf: &'b mut [u8], flags: c_int)
                       -> SockResult<&'b [u8]> {
        let zmq_recv = cfn! {
            fn zmq_recv(socket: *mut c_void,
                        buf: *mut c_void,
                        len: size_t,
                        flags: c_int) -> c_int,
            in self.zmqlib.lib
        };
        let bufptr = buf.as_mut_ptr() as *mut c_void;
        let buflen = buf.len() as size_t;
        match unsafe { zmq_recv(self.ptr, bufptr, buflen, flags) } {
            -1 => match errno_to_error() {
                None => Ok(&[]),
                Some(zmq::Error::EINTR) => Err(SockErr::Interrupted),
                Some(zmq::Error::EAGAIN) => Err(SockErr::TemporarilyUnavailable),
                Some(zmq_error) => {
                    // TODO: use zmq_error
                    Err(SockErr::FailedToReceive)
                }
            },
            num_bytes if num_bytes > buflen as c_int =>
                Err(SockErr::MessageTruncated),
            num_bytes => Ok(&buf[..num_bytes as usize]),
        }
    }

    /// See [zmq_send](http://api.zeromq.org/4-1:zmq_send)
    pub fn send(&self, buf: &[u8], flags: c_int) -> SockResult<c_int> {
        let func = cfn! {
            fn zmq_send(socket: *mut c_void,
                        buf: *const c_void,
                        len: size_t,
                        flags: c_int) -> c_int,
            in self.zmqlib.lib
        };
        let bufptr = buf.as_ptr() as *const c_void;
        let buflen = buf.len() as size_t;
        match unsafe { func(self.ptr, bufptr, buflen, flags) } {
            -1 => Err(SockErr::FailedToSend), // TODO: use `errno_to_error()`
            num_bytes => Ok(num_bytes),
        }
    }

    pub fn close(&self) -> SockResult<()> {
        let func = cfn!{
            fn zmq_close(socket: *mut c_void) -> c_int,
            in self.zmqlib.lib
        };
        match unsafe { func(self.ptr) } {
            0 => Ok(()),
            _ => Err(SockErr::FailedToCloseSocket),
        }
    }
}

impl<'z> Drop for ZmqSocket<'z> {
    fn drop(&mut self) {
        self.close().expect("Could not close socket");
    }
}

/******************************************************************************/
/*                              ZmqLib                                        */
/******************************************************************************/
pub type ZmqLibResult<T> = Result<T, ZmqLibErr>;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ZmqLibErr {
    IoNotFound(Option<String>),
    IoPermissionDenied(Option<String>),
    IoConnectionRefused(Option<String>),
    IoConnectionReset(Option<String>),
    IoConnectionAborted(Option<String>),
    IoNotConnected(Option<String>),
    IoAddrInUse(Option<String>),
    IoAddrNotAvailable(Option<String>),
    IoBrokenPipe(Option<String>),
    IoAlreadyExists(Option<String>),
    IoWouldBlock(Option<String>),
    IoInvalidInput(Option<String>),
    IoInvalidData(Option<String>),
    IoTimedOut(Option<String>),
    IoWriteZero(Option<String>),
    IoInterrupted(Option<String>),
    IoOther(Option<String>),
    IoUnexpectedEof(Option<String>),
}

impl From<io::Error> for ZmqLibErr {
    fn from(ioe: io::Error) -> ZmqLibErr {
        use io::ErrorKind;
        use std::error::Error;
        let cause = ioe.cause().map(|err: &Error| format!("{}", err));
        match ioe.kind() {
            ErrorKind::NotFound => ZmqLibErr::IoNotFound(cause),
            ErrorKind::PermissionDenied => ZmqLibErr::IoPermissionDenied(cause),
            ErrorKind::ConnectionRefused => ZmqLibErr::IoConnectionRefused(cause),
            ErrorKind::ConnectionReset => ZmqLibErr::IoConnectionReset(cause),
            ErrorKind::ConnectionAborted => ZmqLibErr::IoConnectionAborted(cause),
            ErrorKind::NotConnected => ZmqLibErr::IoNotConnected(cause),
            ErrorKind::AddrInUse => ZmqLibErr::IoAddrInUse(cause),
            ErrorKind::AddrNotAvailable => ZmqLibErr::IoAddrNotAvailable(cause),
            ErrorKind::BrokenPipe => ZmqLibErr::IoBrokenPipe(cause),
            ErrorKind::AlreadyExists => ZmqLibErr::IoAlreadyExists(cause),
            ErrorKind::WouldBlock => ZmqLibErr::IoWouldBlock(cause),
            ErrorKind::InvalidInput => ZmqLibErr::IoInvalidInput(cause),
            ErrorKind::InvalidData => ZmqLibErr::IoInvalidData(cause),
            ErrorKind::TimedOut => ZmqLibErr::IoTimedOut(cause),
            ErrorKind::WriteZero => ZmqLibErr::IoWriteZero(cause),
            ErrorKind::Interrupted => ZmqLibErr::IoInterrupted(cause),
            ErrorKind::Other => ZmqLibErr::IoOther(cause),
            ErrorKind::UnexpectedEof => ZmqLibErr::IoUnexpectedEof(cause),
            _ => unimplemented!(),
        }
    }
}


pub struct ZmqLib { lib: lib::Library,  path: PathBuf }

impl ZmqLib {
    pub fn new<'s, S: Into<&'s str>>(soname: S) -> ZmqLibResult<Self> {
        let path = PathBuf::from(soname.into());
        let lib = lib::Library::new(&path)?;
        Ok(ZmqLib { lib: lib, path: path })
    }

    pub fn new_context<'z>(&'z self) -> ZmqLibResult<ZmqCtx<'z>> {
        let func = cfn! { fn zmq_ctx_new() -> *mut c_void,  in &self.lib };
        Ok(ZmqCtx { ptr: unsafe { func() }, lib: &self })
    }

    /// Return the path the lib was loaded from.
    pub fn load_path(&self) -> &Path { &self.path }

}

/******************************************************************************/
/*                              Utilities                                     */
/******************************************************************************/
pub fn as_nanos(d: Duration) -> u64 {
    const NANOS_PER_SEC: u64 = 1_000_000_000;
    d.as_secs() * NANOS_PER_SEC + d.subsec_nanos() as u64
}

#[cfg(target_os = "macos")]
pub fn location() -> &'static str { "/usr/local/lib/libzmq.dylib" }

#[cfg(target_os = "linux")]
pub fn location() -> &'static str {
    let output = process::Command::new("uname")
        .arg("-a")
        .output()
        .expect("Failed to gather OS information");
    let uname = String::from_utf8(output.stdout).unwrap();
    match uname {
        _ if uname.contains("Ubuntu") => "/usr/lib/x86_64-linux-gnu/libzmq.so",
        // TODO: Insert the libzmq sonames for other Linux OSes here.
        _ => "NOT_FOUND", // Other
    }
}

/******************************************************************************/
/*                              Tests                                         */
/******************************************************************************/
#[cfg(test)]
mod tests {
    use super::{ZmqLib, location};

    #[test]
    fn will_it_load() {
        let loc = location();
        assert!(ZmqLib::new(loc).is_ok());
    }
}
