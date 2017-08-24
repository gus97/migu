package com.gus.trace.aop;

public class TraceThreadLocal {

	public static ThreadLocal<TraceInfo> TTL = new ThreadLocal<TraceInfo>();
}
