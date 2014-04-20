/* -*- mode: c; -*- */

/* rpc.x -- Protocol definition for Awget.
 *
 * Definition of the protocol which is used for communication between
 * a Awget client and a server.  The protocol is written in XDR
 * language (see RFC 1832).
 *
 * Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of Awget.
 *
 * Awget is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Awget is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Awget.  If not, see <http://www.gnu.org/licenses/>.
 */

enum result_t {
  SUCCESS =  0,
  ERROR   = -1
};


/* FIXME: Represenation of the link list probably can be
   simplified. */

struct link_t {
  int id;

  /* When the link was added. */
  hyper ts_start;

  /* When the file was downloaded.  -1 means that the file has not
     been downloaded yet. */
  hyper ts_finished;

  string url<>;
};

struct link_list_t
{
  link_t l;                     /* Link structure */
  link_list_t *next;
};

struct get_list_reply_t
{
  link_list_t *links;
};

union get_list_res switch (result_t result) {
case SUCCESS:
	get_list_reply_t reply;
default:
	void;
};


program AWGET_PROGRAM
{
  version AWGET_VERSION
  {
    result_t add_link (string)    = 1;
    result_t rem_link (int)       = 2;
    get_list_res get_list (void)  = 3;
    void quit (void)              = 10;
  } = 0;
} = 25555;

/* rpc.x ends here. */
