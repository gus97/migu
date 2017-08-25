package com.gus.trace.aop;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.net.InetAddress;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HttpServiceFilter implements Filter {

	private final static Logger logger = LoggerFactory.getLogger(HttpServiceFilter.class);

	@Override
	public void destroy() {

	}

	@Override
	public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain chain) {

		TraceInfo before;

		TraceInfo after;

		String ip = null;

		try {
			HttpServletRequest request = (HttpServletRequest) servletRequest;

			String requestUrl = request.getRequestURI();

			int spanSuffix = (int) (Math.random() * (9999 - 1000 + 1)) + 1000;

			ip = InetAddress.getLocalHost().getHostAddress();

			// 表示根
			if (requestUrl.indexOf("favicon.ico") == -1) {
				if (request.getHeader("trace-id") == null) {

					before = new TraceInfo(new DistributedIDS(2).nextId(), System.currentTimeMillis() + spanSuffix,
							null, requestUrl, "SR", System.currentTimeMillis(), ip, null);
				} else {
					before = new TraceInfo(Long.parseLong(request.getHeader("trace-id")),
							System.currentTimeMillis() + spanSuffix, Long.parseLong(request.getHeader("span-id")),
							requestUrl, "SR", System.currentTimeMillis(), ip, null);
				}

				TraceThreadLocal.TTL.set(before);
				logger.info(before.toString());

				chain.doFilter(servletRequest, servletResponse);

				after = new TraceInfo(TraceThreadLocal.TTL.get().traceID, TraceThreadLocal.TTL.get().spanID,
						TraceThreadLocal.TTL.get().parentID, TraceThreadLocal.TTL.get().url, "SS",
						System.currentTimeMillis(), ip, null);

				logger.info(after.toString());
			}
		} catch (Exception e) {
			after = new TraceInfo(TraceThreadLocal.TTL.get().traceID, TraceThreadLocal.TTL.get().spanID,
					TraceThreadLocal.TTL.get().parentID, TraceThreadLocal.TTL.get().url, "SS",
					System.currentTimeMillis(), ip, getStackTrace(e));

			logger.info(after.toString());
		}
	}

	@Override
	public void init(FilterConfig arg0) throws ServletException {

	}

	public static String getStackTrace(Throwable aThrowable) {
		final Writer result = new StringWriter();
		final PrintWriter printWriter = new PrintWriter(result);
		aThrowable.printStackTrace(printWriter);
		return result.toString();
	}
}
