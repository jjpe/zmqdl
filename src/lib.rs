extern crate libc;
extern crate libloading as lib;
extern crate zmq;

use libc::{c_char, c_int, c_void, size_t};
use std::ffi::CString;
use std::io;
use std::mem;
use std::path::{Path, PathBuf};
use std::time::Duration;
#[cfg(target_os = "linux")] use std::process;
pub use zmq::{Constants, SocketType};

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


macro_rules! cfn {
    (fn $name:ident($($args:ident : $argtypes:ty),*) -> $ret:ty,
     in $lib:expr) => {{
         let name = stringify!($name);
         let name_bytes = name.as_bytes();
         type FnSig = unsafe extern fn ($($argtypes),*) -> $ret;
         unsafe { try!($lib.get(name_bytes)) as lib::Symbol<FnSig> }
     }};
}


pub struct ZmqCtx<'z> { ptr: *mut c_void, lib: &'z ZmqLib, }

impl<'z> ZmqCtx<'z> {
    /// See [zmq-socket](http://api.zeromq.org/4-1:zmq-socket)
    pub fn socket(&self, stype: SocketType) -> io::Result<ZmqSocket> {
        let func = cfn! {
            fn zmq_socket(ctx: *mut c_void, stype: c_int) -> *mut c_void,
            in self.lib.lib
        };
        let sock = unsafe { ZmqSocket {
            ptr: func(self.ptr, stype as c_int),
            lib: &self.lib
        }};
        Ok(sock)
    }

    pub fn term(&self) -> io::Result<()> {
        let term = cfn! {
            fn zmq_term(context: *mut c_void) -> c_int,
            in self.lib.lib
        };
        unsafe { match term(self.ptr) {
            0 => Ok(()),
            rc => {
                let msg = format!("Could not terminate: {:?}", rc);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            },
        }}
    }
}

impl<'z> Drop for ZmqCtx<'z> {
    fn drop(&mut self) {
        self.term().expect("Could not terminate context");
    }
}


pub struct ZmqSocket<'z> { ptr: *mut c_void, lib: &'z ZmqLib, }

impl<'z> ZmqSocket<'z> {
    fn set_option(&self,
                  option_name: Constants,
                  optvalue: *const c_void,
                  optlen: size_t) -> io::Result<()> {
        let func = cfn! {
            fn zmq_setsockopt(socket: *mut c_void,
                              option_name: c_int,
                              option_value: *const c_void,
                              option_len: size_t) -> c_int,
            in self.lib.lib
        };
        unsafe { match func(self.ptr, option_name as c_int, optvalue, optlen) {
            0 => Ok(()),
            _ => Err(io::Error::new(io::ErrorKind::Other,
                                    "Setting socket option failed")),
        }}
    }


    pub fn set_linger_period(&self, millis: c_int) -> io::Result<()> {
        self.set_option(Constants::ZMQ_LINGER,
                        (&millis as *const c_int) as *const c_void,
                        mem::size_of::<c_int>())
    }

    /// See [zmq-connect](http://api.zeromq.org/4-1:zmq-connect)
    pub fn connect(&self, addr: &str) -> io::Result<c_int> {
        let func = cfn! {
            fn zmq_connect(socket: *mut c_void, addr: *const c_char) -> c_int,
            in self.lib.lib
        };
        let addr_ptr = CString::new(addr).unwrap().into_raw();
        unsafe { match func(self.ptr, addr_ptr) {
            -1 => Err(io::Error::new(io::ErrorKind::ConnectionAborted,
                                     "Error connecting")),
            rc => Ok(rc),
        }}
    }

    /// See [zmq-bind](http://api.zeromq.org/4-1:zmq-bind)
    pub fn bind(&self, addr: &str) -> io::Result<c_int> {
        let func = cfn! {
            fn zmq_bind(socket: *mut c_void, addr: *const c_char) -> c_int,
            in self.lib.lib
        };
        let addr_ptr = CString::new(addr).unwrap().into_raw();
        match unsafe { func(self.ptr, addr_ptr) } {
            -1 => Err(io::Error::new(io::ErrorKind::ConnectionAborted,
                                     "Error binding")),
            rc => Ok(rc),
        }
    }

    /// See [zmq_recv](http://api.zeromq.org/4-1:zmq_recv)
    pub fn receive<'b>(&self, buf: &'b mut [u8], flags: c_int)
                       -> io::Result<&'b mut [u8]> {
        let func = cfn! {
            fn zmq_recv(socket: *mut c_void,
                        buf: *mut c_void,
                        len: size_t,
                        flags: c_int) -> c_int,
            in self.lib.lib
        };
        let bufptr = buf.as_mut_ptr() as *mut c_void;
        let buflen = buf.len() as size_t;
        match unsafe { func(self.ptr, bufptr, buflen, flags) } {
            -1 => Err(io::Error::new(io::ErrorKind::Other, "Receive failed")),
            num_bytes if num_bytes > buflen as c_int =>
                Err(io::Error::new(io::ErrorKind::InvalidData, "Msg truncated")),
            num_bytes => Ok(&mut buf[..num_bytes as usize]),
        }
    }

    /// See [zmq_send](http://api.zeromq.org/4-1:zmq_send)
    pub fn send(&self, buf: &mut [u8], flags: c_int) -> io::Result<c_int> {
        let func = cfn! {
            fn zmq_send(socket: *mut c_void,
                        buf: *mut c_void,
                        len: size_t,
                        flags: c_int) -> c_int,
            in self.lib.lib
        };
        let bufptr = buf.as_mut_ptr() as *mut c_void;
        let buflen = buf.len() as size_t;
        Ok(unsafe { func(self.ptr, bufptr, buflen, flags) })
    }

    pub fn close(&self) -> io::Result<()> {
        let func = cfn!{
            fn zmq_close(socket: *mut c_void) -> c_int,
            in self.lib.lib
        };
        match unsafe { func(self.ptr) } {
            0 => Ok(()),
            _ => {
                let msg = "Could not properly close socket";
                Err(io::Error::new(io::ErrorKind::Other, msg))
            },
        }
    }

}

impl<'z> Drop for ZmqSocket<'z> {
    fn drop(&mut self) {
        self.close().expect("Could not close socket");
    }
}


pub struct ZmqLib { lib: lib::Library, path: PathBuf, }

impl ZmqLib {
    pub fn new<'s, S: Into<&'s str>>(soname: S) -> io::Result<Self> {
        let path = PathBuf::from(soname.into());
        let lib = try!(lib::Library::new(&path));
        Ok(ZmqLib { lib: lib, path: path })
    }

    pub fn new_context(&self) -> io::Result<ZmqCtx> {
        let func = cfn! { fn zmq_ctx_new() -> *mut c_void,  in &self.lib };
        Ok(ZmqCtx { ptr: unsafe { func() }, lib: &self })
    }

    /// Return the path the lib was loaded from.
    pub fn load_path(&self) -> &Path { &self.path }

}



#[cfg(test)]
mod tests {
    use super::{ZmqLib, location};

    #[test]
    fn will_it_load() {
        let loc = location();
        assert!(ZmqLib::new(loc).is_ok());
    }
}
