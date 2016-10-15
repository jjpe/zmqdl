extern crate libc;
extern crate libloading as lib;
extern crate zmq;

use libc::{c_char, c_int, c_void, size_t};
use std::ffi::CString;
use std::io;
use std::path::{Path, PathBuf};
#[cfg(target_os = "linux")] use std::process;
pub use zmq::SocketType;


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
         unsafe { try!((&$lib).get(name_bytes)) as lib::Symbol<FnSig> }
     }};
}


pub struct ZmqCtx(*mut c_void);
pub struct ZmqSocket(*mut c_void);
pub struct ZmqLib { lib: lib::Library, path: PathBuf }

impl ZmqLib {
    pub fn new<'s, S: Into<&'s str>>(soname: S) -> io::Result<Self> {
        let path = PathBuf::from(soname.into());
        let lib = try!(lib::Library::new(&path));
        println!("Loaded zmq @ {}", &path.to_str().unwrap());
        Ok(ZmqLib { lib: lib, path: path })
    }

    /// Return the path the lib was loaded from.
    pub fn load_path(&self) -> &Path { &self.path }

    /// See [zmq-ctx-new](http://api.zeromq.org/4-1:zmq-ctx-new)
    pub fn context(&self) -> io::Result<ZmqCtx> {
        let func = cfn!{ fn zmq_ctx_new() -> *mut c_void,  in self.lib };
        Ok(ZmqCtx(unsafe { func() }))
    }

    /// See [zmq-socket](http://api.zeromq.org/4-1:zmq-socket)
    pub fn socket(&self, context: ZmqCtx, socket_type: SocketType)
                      -> io::Result<ZmqSocket> {
        let func = cfn! {
            fn zmq_socket(ctx: *mut c_void, stype: c_int) -> *mut c_void,
            in self.lib
        };
        Ok(ZmqSocket(unsafe { func(context.0, socket_type as c_int) }))
    }

    /// See [zmq-connect](http://api.zeromq.org/4-1:zmq-connect)
    pub fn connect(&self, socket: &ZmqSocket, addr: &str) -> io::Result<c_int> {
        let func = cfn! {
            fn zmq_connect(socket: *mut c_void, addr: *const c_char) -> c_int,
            in self.lib
        };
        let addr_ptr = CString::new(addr).unwrap().into_raw();
        match unsafe { func(socket.0, addr_ptr) } {
            -1 => Err(io::Error::new(io::ErrorKind::ConnectionAborted,
                                     "Error connecting")),
            rc => Ok(rc),
        }
    }

    /// See [zmq-bind](http://api.zeromq.org/4-1:zmq-bind)
    pub fn bind(&self, socket: &ZmqSocket, addr: &str) -> io::Result<c_int> {
        let func = cfn! {
            fn zmq_bind(socket: *mut c_void, addr: *const c_char) -> c_int,
            in self.lib
        };
        let addr_ptr = CString::new(addr).unwrap().into_raw();
        match unsafe { func(socket.0, addr_ptr) } {
            -1 => Err(io::Error::new(io::ErrorKind::ConnectionAborted,
                                     "Error binding")),
            rc => Ok(rc),
        }
    }

    /// See [zmq_recv](http://api.zeromq.org/4-1:zmq_recv)
    pub fn receive(&self, sock: &ZmqSocket, buf: &mut [u8], flags: c_int)
                   -> io::Result<c_int> {
        let func = cfn! {
            fn zmq_recv(socket: *mut c_void,
                        buf: *mut c_void,
                        len: size_t,
                        flags: c_int) -> c_int,
            in self.lib
        };
        let bufptr = buf.as_mut_ptr() as *mut c_void;
        let buflen = buf.len() as size_t;
        Ok(unsafe { func(sock.0, bufptr, buflen, flags) })
    }

    /// See [zmq_send](http://api.zeromq.org/4-1:zmq_send)
    pub fn send(&self, sock: &ZmqSocket, buf: &mut [u8], flags: c_int)
                -> io::Result<c_int> {
        let func = cfn! {
            fn zmq_send(socket: *mut c_void,
                        buf: *mut c_void,
                        len: size_t,
                        flags: c_int) -> c_int,
            in self.lib
        };
        let bufptr = buf.as_mut_ptr() as *mut c_void;
        let buflen = buf.len() as size_t;
        Ok(unsafe { func(sock.0, bufptr, buflen, flags) })
    }
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
