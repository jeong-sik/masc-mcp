type addr = {
  addr: string;
  port: int;
}

type config = {
  bind_addr: string;
  bind_port: int;
  recv_buffer_size: int;
}

type dgram = {
  data: bytes;
  source: addr;
}

type socket = Socket : 'a Eio.Net.datagram_socket -> socket

type t = {
  socket: socket;
  recv_buf_size: int;
}

let default_config = {
  bind_addr = "0.0.0.0";
  bind_port = 0;
  recv_buffer_size = 262_144;
}

let ipaddr_of_string s =
  match Ipaddr.of_string s with
  | Ok ip ->
    Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets ip)
  | Error _ ->
    Eio.Net.Ipaddr.V4.any

let sockaddr_of_addr (a : addr) : Eio.Net.Sockaddr.datagram =
  `Udp (ipaddr_of_string a.addr, a.port)

let addr_of_sockaddr (sa : Eio.Net.Sockaddr.datagram) : addr =
  match sa with
  | `Udp (ip, port) ->
    { addr = Format.asprintf "%a" Eio.Net.Ipaddr.pp ip; port }
  | `Unix path ->
    { addr = path; port = 0 }

let create ~net ~sw ~config () =
  let bind_addr = sockaddr_of_addr { addr = config.bind_addr; port = config.bind_port } in
  let socket = Eio.Net.datagram_socket ~sw net bind_addr in
  let recv_buf_size = max 2048 config.recv_buffer_size in
  { socket = Socket socket; recv_buf_size }

let bind _t = ()

let close t =
  let (Socket sock) = t.socket in
  Eio.Resource.close sock

let send_to t (data : bytes) (dest : addr) =
  let dst = sockaddr_of_addr dest in
  let buf = Cstruct.of_bytes data in
  let (Socket sock) = t.socket in
  Eio.Net.send sock ~dst [buf]

let recv_timeout t timeout_sec =
  let recv_once () =
    let buf = Cstruct.create t.recv_buf_size in
    let (Socket sock) = t.socket in
    let (src, len) = Eio.Net.recv sock buf in
    let data = Bytes.create len in
    Cstruct.blit_to_bytes buf 0 data 0 len;
    Some { data; source = addr_of_sockaddr src }
  in
  let timeout () =
    if timeout_sec > 0.0 then Eio_unix.sleep timeout_sec;
    None
  in
  Eio.Fiber.first recv_once timeout
