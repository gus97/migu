package cn.migu.file.core;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class SqlConstant {
	// public final static String QUERY_FLUME_FILES = "SELECT
	// obj_id,init_path,collect_path,code,character FROM flume_file WHERE app_id
	// = ? AND server_id = ? AND jar_id = ? ";

	public final static String QUERY_FLUME_FILES = "SELECT t1.obj_id, t1.init_path, t1.collect_path, t1.code, t1.character,T1.kind,T1.IS_VERIFY,t1.decollator,t1.col_num,t1.START_LINE,t1.EXT_FUNCTION FROM flume_file t1, unify_jar_file t2 WHERE t2.app_id = ? AND t2.server_id = ? AND t2.jar_id = ? And t2.file_id = t1.obj_id order by t1.init_path";

	// 没有采集时间的直接忽略
	public final static String QUERY_FLUME_LOGS = "SELECT distinct fname FROM flume_file_log WHERE app_id = ? AND server_id = ? AND jar_id = ?  and TO_DATE(TO_CHAR(sysdate - ?, 'yyyy-mm-dd hh24:mi:ss'), 'YYYY-MM-DD HH24:MI:SS') < copy_time ";

	public final static String INSERT_FLIME_LOGS = "INSERT INTO flume_file_log (obj_id,app_id,server_id,jar_id,file_id,fname,bname,file_size,records,collect_path,copy_time,collect_user,encode) VALUES (S_FLIME_FILE_LOG.nextval,?,?,?,?,?,?,?,?,?,to_date(?,'yyyy-mm-dd hh24:mi:ss'),'flume',?)";

	public final static String HEARTBEAT_CONNECT_INSERT = "INSERT INTO flume_file_hb values (S_FLUME_FILE_HB.nextval,?,?,?,?)";

	public final static String HEARTBEAT_CONNECT_UPDATE = "UPDATE flume_file_hb SET h_date = ? where app_id = ? and server_id = ? and jar_id = ?";

	public final static String HEARTBEAT_CONNECT_INTERVAL = "SELECT h_date cnt FROM flume_file_hb WHERE app_id = ? AND server_id = ? AND jar_id = ?";

	public final static String IS_UTF_8 = "1.0";

	public static String QUERYFORLISTURL = "";

	public static String EXECUTEURL = "";

	public final static String INSERT_PROCESS = "INSERT INTO UNIFY_PROCESS (OBJ_ID, APP_ID, SERVER_ID, JAR_ID, PORT, KIND, PROCESS_NO, STATUS, DEAL_TIME, DEAL_USER,NOTE) VALUES (?,?,?,?,?,3,?,1,sysdate,?,?)";

	public final static String INSERT_PROCESS_LOG = "INSERT INTO UNIFY_PROCESS_LOG (OBJ_ID, APP_ID, SERVER_ID, JAR_ID, PORT, KIND, PROCESS_NO, STATUS, DEAL_TIME, DEAL_USER,NOTE) VALUES (?,?,?,?,?,3,?,1,sysdate,?,?)";

	public final static String INSERT_VERIFY_FILE = "INSERT INTO unify_verify_file values (?,?,?,?,?,?,?,?,?,?) ";

	public final static String DELETE_VERIFY_FILE = "DELETE FROM unify_verify_file WHERE SERVER_ID=? and NAME=? ";

	public final static String QUERY_FLUME_SERVER = "SELECT FS.IP SERVERIP,FS.USERNAME,FS.PASSWORD PWD from flume_server fs where FS.OBJ_ID= ? ";

	public final static String QUERY_VERIFY_FILE = "SELECT * from unify_verify_file uvf where UVF.FILE_ID = ? AND UVF.SERVER_ID = ? ";

	public static String SERVER_IP = "0.0.0.0";

	public static Long rollTime = 1000 * 60 * 2L;
	
	public static Integer sqlRollTime = (int) Math.ceil(120 / 24 / 60D);

	static {
		try {
			SERVER_IP = InetAddress.getLocalHost().getHostAddress();
		} catch (UnknownHostException e) {
			e.printStackTrace();
		}
	}
}
